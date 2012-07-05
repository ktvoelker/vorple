
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Vorple
  ( Vorple()
  , runVorple
  , throwError
  ) where

import Codec.Utils
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as Ae
import Data.Aeson.TH
import Data.Aeson.Types
-- import qualified Data.Attoparsec as At
-- import qualified Data.ByteString.Char8 as BS
-- import Data.List
-- import Data.Maybe
-- import qualified Data.Text.Lazy as T
-- import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), status401, status500)
import Network.Wai (requestHeaders, Application())
import System.Random
import qualified Web.Scotty as WS

instance Error Status where
  noMsg = status500

newtype Vorple e s a = Vorple { getv :: ErrorT Status (ReaderT e (StateT s IO)) a }
  deriving (Monad)

instance MonadError Status (Vorple e s) where
  throwError = Vorple . throwError
  catchError m f = Vorple $ getv m `catchError` (getv . f)

instance MonadReader e (Vorple e s) where
  ask = Vorple ask
  local f = Vorple . local f . getv
  reader = Vorple . reader

instance MonadState s (Vorple e s) where
  get = Vorple get
  put = Vorple . put

instance MonadIO (Vorple e s) where
  liftIO = Vorple . liftIO

data Hmac = Hmac
  { hmacSum  :: [Octet]
  , hmacData :: [Octet]
  }

data Csrf a = Csrf
  { csrfKey  :: Int
  , csrfData :: a
  }

$(deriveJSON (drop 4) ''Csrf)

-- TODO Read and Show instances of Hmac and Csrf types

readMaybe :: (Read a) => String -> Maybe a
readMaybe xs = case reads xs of
  ((a, "") : _) -> Just a
  _ -> Nothing

runVorple
  :: (FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)
  -- The secret application key
  => [Octet]
  -- The initial environmentn
  -> e
  -- The default session state
  -> s
  -- The request handler
  -> (a -> Vorple e s b)
  -- The application
  -> IO Application
runVorple appKey env emptySession handler = WS.scottyApp $ WS.post "/" $ do
  input <- WS.jsonData
  let error = getv $ handler input
  let reader = runErrorT error
  -- TODO: read the initial state from the cookie and write the result state back
  -- TODO: check the CSRF key from the cookie against one wrapping the request
  -- TODO: HMAC the session with the application key so we can trust it
  -- TODO: before HMACing, wrap the data in a CSRF key
  let state = runReaderT reader env
  (response, nextSession) <- liftIO $ runStateT state emptySession
  either WS.status WS.json response

{-
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

setUser :: (MonadAction m) => Int -> m ()
setUser u = liftActionM $ makeSecret >>= setCookie . Cookie (User u)

getUser :: Vorple e Int
getUser = do
  c <- liftActionM getCookie
  case c of
    Nothing -> throwError status401
    Just (Cookie (User u) s) -> do
      a <- liftActionM $ param "secret"
      if Secret a == s
      then return u
      else throwError status401
-}

