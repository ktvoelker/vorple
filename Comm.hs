
{-# LANGUAGE OverloadedStrings #-}
module Comm where

import Control.Monad.Reader
import Control.Monad.Trans
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), unauthorized401, StdMethod())
import Network.Wai (requestHeaders)
import System.Random
import Web.Scotty

class ToActionM m where
  toActionM :: m a -> ActionM a

instance ToActionM ActionM where
  toActionM = id

instance ToActionM IO where
  toActionM = liftIO

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

result :: (ToJSON a, ToActionM m) => ReaderT e m a -> ReaderT e ActionM ()
result r = mapReaderT toActionM r >>= lift . json

cookieMarker :: String
cookieMarker = "session="

cookieMarkerLength :: Int
cookieMarkerLength = length cookieMarker

readMaybe :: (Read a) => String -> Maybe a
readMaybe xs = case reads xs of
  ((a, "") : _) -> Just a
  _ -> Nothing

getCookie :: ActionM (Maybe Cookie)
getCookie = do
  r <- request
  return
    $ listToMaybe
    $ take 1
    $ catMaybes
    $ map (readMaybe . drop cookieMarkerLength)
    $ filter (cookieMarker `isPrefixOf`)
    $ map (BS.unpack . snd)
    $ filter ((== "Cookie") . fst)
    $ requestHeaders r

setCookie :: Cookie -> ActionM ()
setCookie =
  header "Set-Cookie"
  . (fromString cookieMarker `T.append`)
  . fromString
  . show

makeSecret :: ActionM Secret
makeSecret = liftIO (getStdRandom random) >>= return . Secret

maybeJsonData :: (FromJSON a) => ReaderT e ActionM (Maybe a)
maybeJsonData = lift $ rescue (jsonData >>= return . Just) (const $ return Nothing)

noAuth
  :: (FromJSON a, ToJSON b, ToActionM m)
  => (Maybe a -> ReaderT e m b)
  -> ReaderT e ActionM ()
noAuth = (maybeJsonData >>=) . (result .)

doesAuth
  :: (FromJSON a, ToJSON b, ToActionM m)
  => (Maybe a -> ReaderT e m (User, b))
  -> ReaderT e ActionM ()
doesAuth h = do
  (u, r) <- maybeJsonData >>= mapReaderT toActionM . h
  lift $ do
    makeSecret >>= setCookie . Cookie u
    json r

mustAuth
  :: (FromJSON a, ToJSON b, ToActionM m)
  => (User -> Maybe a -> ReaderT e m b)
  -> ReaderT e ActionM ()
mustAuth h = do
  j <- maybeJsonData
  c <- lift getCookie
  case c of
    Nothing -> lift $ status unauthorized401
    Just (Cookie u s) -> do
      a <- lift $ param "secret"
      if Secret a == s
      then result $ h u j
      else lift $ status unauthorized401

