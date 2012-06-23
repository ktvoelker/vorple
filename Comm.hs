
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
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

data Handler a b =
    RequireAuth   (User -> a -> IO b)
  | DoesAuth      (a -> IO (User, b))
  | NoRequireAuth (a -> IO b)

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

runHandler :: (FromJSON a, ToJSON b) => Handler a b -> ActionM ()
runHandler h = do
  (j :: b) <- jsonData
  case h of
    NoRequireAuth h -> result $ h j
    DoesAuth h -> do
      (u, r) <- liftIO $ h j
      s <- makeSecret
      setCookie $ Cookie u s
      json r
    RequireAuth h -> do
      c <- getCookie
      case c of
        Nothing -> status unauthorized401
        Just (Cookie u s) -> do
          a <- param "secret"
          if Secret a == s
          then result $ h u j
          else status unauthorized401

serve
  :: (FromJSON a, ToJSON b)
  => StdMethod -> RoutePattern -> Handler a b -> ScottyM ()
serve method route handler = addroute method route $ runHandler handler

