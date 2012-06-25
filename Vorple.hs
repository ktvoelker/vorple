
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vorple
  ( MonadAction()
  , Vorple()
  , serve
  , run
  , param
  , jsonParam
  , parseJson
  , jsonData
  , setUser
  , getUser
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Aeson as Ae
import Data.Aeson.Types
import qualified Data.Attoparsec as At
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), status400, status401, status500)
import Network.Wai (requestHeaders)
import System.Random
import Web.Scotty (ActionM, header, request, json, status)
import qualified Web.Scotty as S

import Vorple.Unsafe

instance Error Status where
  noMsg = status500

newtype Vorple e a = Vorple { getv :: ErrorT Status (ReaderT e ActionM) a }
  deriving (Monad, MonadAction)

instance MonadError Status (Vorple e) where
  throwError = Vorple . throwError
  catchError m f = Vorple $ getv m `catchError` (getv . f)

instance MonadReader e (Vorple e) where
  ask = Vorple ask
  local f = Vorple . local f . getv
  reader = Vorple . reader

instance MonadIO (Vorple e) where
  liftIO = Vorple . liftIO

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

serve :: (ToJSON a) => (ActionM () -> m ()) -> Vorple e a -> ReaderT e m ()
serve f = mapReaderT f . run

run :: (ToJSON a) => Vorple e a -> ReaderT e ActionM ()
run = runErrorT . getv >=> lift . either status json

param :: (S.Parsable a) => T.Text -> Vorple e a
param = Vorple . lift . lift . S.param

jsonParam :: forall a e. (FromJSON a) => T.Text -> Vorple e a
jsonParam = (param :: T.Text -> Vorple e BS.ByteString) >=> parseJson

parseJson :: forall a e. (FromJSON a) => BS.ByteString -> Vorple e a
parseJson bs = case At.parse Ae.json bs :: At.Result Value of
  At.Done "" r -> case fromJSON r of
    Success r -> return r
    _ -> throwError status400
  _ -> throwError status400

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

jsonData :: (FromJSON a) => Vorple e a
jsonData = liftActionM S.jsonData

setUser :: (MonadAction m) => User -> m ()
setUser u = liftActionM $ makeSecret >>= setCookie . Cookie u

getUser :: Vorple e User
getUser = do
  c <- liftActionM getCookie
  case c of
    Nothing -> throwError status401
    Just (Cookie u s) -> do
      a <- param "secret"
      if Secret a == s
      then return u
      else throwError status401

