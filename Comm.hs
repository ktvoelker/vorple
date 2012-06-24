
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Comm where

import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), status500, status401, StdMethod())
import Network.Wai (requestHeaders)
import System.Random
import Web.Scotty

instance Error Status where
  noMsg = status500

class (Monad m) => MonadAction m where
  liftActionM :: ActionM a -> m a

instance MonadAction ActionM where
  liftActionM = id

instance (MonadAction m) => MonadAction (ReaderT e m) where
  liftActionM = lift . liftActionM

instance (Error e, MonadAction m) => MonadAction (ErrorT e m) where
  liftActionM = lift . liftActionM

newtype Vorple e a = Vorple { getv :: ErrorT Status (ReaderT e ActionM) a }
  deriving (Monad, MonadAction)

instance MonadError Status (Vorple e) where
  throwError = Vorple . throwError
  catchError m f = Vorple $ getv m `catchError` (getv . f)

instance MonadReader e (Vorple e) where
  ask = Vorple ask
  local f = Vorple . local f . getv
  reader = Vorple . reader

class EnvAdd a e f | a e -> f where
  envAdd :: a -> e -> f

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

run :: (ToJSON a, MonadAction m, MonadError Status m) => m a -> m ()
run = (`catchError` (liftActionM . status)) . (>>= liftActionM . json)

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

mapEnv :: (e -> f) -> Vorple f a -> Vorple e a
mapEnv f = Vorple . mapErrorT (withReaderT f) . getv

withJson :: forall a b e f m. (FromJSON a, EnvAdd a e f) => Vorple f b -> Vorple e b
withJson m = do
  j <- liftActionM jsonData
  mapEnv (envAdd (j :: a)) m

doesAuth :: (MonadAction m) => m (User, b) -> m b
doesAuth m = do
  (u, x) <- m
  liftActionM $ makeSecret >>= setCookie . Cookie u
  return x

mustAuth :: (EnvAdd User e f) => Vorple f b -> Vorple e b
mustAuth m = do
  c <- liftActionM getCookie
  case c of
    Nothing -> throwError status401
    Just (Cookie u s) -> do
      a <- liftActionM $ param "secret"
      if Secret a == s
      then mapEnv (envAdd u) m
      else throwError status401

