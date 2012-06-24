
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Comm where

import Control.Monad.Error
import Control.Monad.Reader
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

class (Monad m) => MonadAction m where
  liftActionM :: ActionM a -> m a

instance MonadAction ActionM where
  liftActionM = id

instance (MonadAction m) => MonadAction (ReaderT e m) where
  liftActionM = lift . liftActionM

instance (Error e, MonadAction m) => MonadAction (ErrorT e m) where
  liftActionM = lift . liftActionM

class EnvAdd a e f | a e -> f where
  envAdd :: a -> e -> f

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

-- TODO catch status exceptions
run :: (ToJSON a, MonadAction m, MonadReader e m) => m a -> m ()
run = (>>= liftActionM . json)

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

withJson
  :: forall a b e f m. (FromJSON a, EnvAdd a e f, MonadAction m)
  => ReaderT f m b
  -> ReaderT e m b
withJson m = do
  j <- liftActionM jsonData
  e <- ask
  lift $ runReaderT m $ envAdd (j :: a) e

doesAuth :: (MonadAction m) => m (User, b) -> m b
doesAuth m = do
  (u, x) <- m
  liftActionM $ makeSecret >>= setCookie . Cookie u
  return x

mustAuth
  :: (EnvAdd User e f, MonadAction m)
  => ReaderT f m b
  -> ReaderT e m b
mustAuth m = do
  c <- liftActionM getCookie
  case c of
    Nothing -> undefined --liftIO $ throw unauthorized401
    Just (Cookie u s) -> do
      a <- liftActionM $ param "secret"
      if Secret a == s
      then ask >>= lift . runReaderT m . envAdd u
      else undefined --liftIO $ throw unauthorized401

