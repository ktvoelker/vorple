
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
  , runVorple
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
import Network.HTTP.Types (Status(), status401, status500)
import Network.Wai (requestHeaders, Application())
import System.Random
import Web.Scotty

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

runVorple :: (FromJSON a, ToJSON b) => e -> (a -> Vorple e b) -> IO Application
runVorple env =
  scottyApp
  . post "/"
  . flip runReaderT env
  . (runErrorT . getv >=> lift . either status json)
  . (liftActionM jsonData >>=)

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

setUser :: (MonadAction m) => User -> m ()
setUser u = liftActionM $ makeSecret >>= setCookie . Cookie u

getUser :: Vorple e User
getUser = do
  c <- liftActionM getCookie
  case c of
    Nothing -> throwError status401
    Just (Cookie u s) -> do
      a <- liftActionM $ param "secret"
      if Secret a == s
      then return u
      else throwError status401

