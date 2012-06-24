
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Comm where

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

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

result :: (ToJSON a) => IO a -> ActionM ()
result r = liftIO r >>= json

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

noAuth :: (FromJSON a, ToJSON b) => (a -> IO b) -> ActionM ()
noAuth = (jsonData >>=) . (result .)

doesAuth :: (FromJSON a, ToJSON b) => (a -> IO (User, b)) -> ActionM ()
doesAuth h = do
  (u, r) <- jsonData >>= liftIO . h
  makeSecret >>= setCookie . Cookie u
  json r

mustAuth :: (FromJSON a, ToJSON b) => (User -> a -> IO b) -> ActionM ()
mustAuth h = do
  j <- jsonData
  c <- getCookie
  case c of
    Nothing -> status unauthorized401
    Just (Cookie u s) -> do
      a <- param "secret"
      if Secret a == s
      then result $ h u j
      else status unauthorized401

