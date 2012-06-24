
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

type M e a = ReaderT e IO a

type M' e a = ReaderT e ActionM a

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

result :: (ToJSON a) => M e a -> M' e ()
result r = mapReaderT liftIO r >>= lift . json

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

noAuth :: (FromJSON a, ToJSON b) => (a -> M e b) -> M' e ()
noAuth = (lift jsonData >>=) . (result .)

doesAuth :: (FromJSON a, ToJSON b) => (a -> M e (User, b)) -> M' e ()
doesAuth h = do
  (u, r) <- lift jsonData >>= mapReaderT liftIO . h
  lift $ do
    makeSecret >>= setCookie . Cookie u
    json r

mustAuth :: (FromJSON a, ToJSON b) => (User -> a -> M e b) -> M' e ()
mustAuth h = do
  j <- lift jsonData
  c <- lift getCookie
  case c of
    Nothing -> lift $ status unauthorized401
    Just (Cookie u s) -> do
      a <- lift $ param "secret"
      if Secret a == s
      then result $ h u j
      else lift $ status unauthorized401

