
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Web.Vorple
  ( Vorple()
  , Options(..)
  , defaultOptions
  , runVorple
  , runVorpleIO
  , runVorpleIdentity
  , logs
  , logp
  , logj
  , throwError
  , ask
  , asks
  , get
  , put
  , modify
  , liftIO
  ) where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.HMAC
import Data.List
import Data.List.Split
import Data.Maybe
import Network.HTTP.Types (Status(), status400, status401, status500)
import Network.Wai (requestHeaders, Application())
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Random
import qualified Web.Scotty as WS

import Web.Vorple.Text

data Options = Options
  -- Enable internal debug logging
  { optDebug  :: Bool
  -- The secret application key
  , optAppKey :: Maybe [Word8]
  } deriving (Eq, Ord, Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optDebug  = True
  , optAppKey = Nothing
  }

newtype OptionsT m a = OptionsT { getOptionsT :: ReaderT Options m a }

instance (Monad m) => Monad (OptionsT m) where
  return = OptionsT . return
  OptionsT m >>= f = OptionsT $ m >>= getOptionsT . f
  fail = OptionsT . fail

instance MonadTrans OptionsT where
  lift = OptionsT . lift

instance (MonadIO m) => MonadIO (OptionsT m) where
  liftIO = OptionsT . liftIO

asksOpt :: (Monad m) => (Options -> a) -> Vorple e s m a
asksOpt = Vorple . lift . lift . lift . lift . OptionsT . asks

instance Error Status where
  noMsg = status500

data Vorple e s m a = Vorple
  { getv :: ErrorT Status
            (WriterT ByteString
            (ReaderT e
            (StateT s
            (OptionsT m)))) a }

instance (Monad m) => Monad (Vorple e s m) where
  return = Vorple . return
  Vorple m >>= f = Vorple $ m >>= getv . f
  fail = Vorple . fail

instance MonadTrans (Vorple e s) where
  lift = Vorple . lift . lift . lift . lift . lift

instance (Monad m) => MonadError Status (Vorple e s m) where
  throwError     = Vorple . throwError
  catchError m f = Vorple $ getv m `catchError` (getv . f)

instance (Monad m) => MonadReader e (Vorple e s m) where
  ask     = Vorple ask
  local f = Vorple . local f . getv
  reader  = Vorple . reader

instance (Monad m) => MonadState s (Vorple e s m) where
  get = Vorple get
  put = Vorple . put

instance (Monad m) => MonadWriter String (Vorple e s m) where
  writer = Vorple . writer
  tell   = Vorple . tell
  listen = Vorple . listen . getv
  pass   = Vorple . pass . getv

instance (MonadIO m) => MonadIO (Vorple e s m) where
  liftIO = Vorple . liftIO

logs :: (Monad m) => Text -> Vorple e s m ()
logs = (>> tell "\n") . tell . encodeUtf8

logp :: (Monad m, Show a) => a -> Vorple e s m ()
logp = logs . packString . show

logj :: (Monad m, ToJSON a) => a -> Vorple e s m ()
logj = (>> tell "\n") . tell . encodeJSON

data Hmac = Hmac
  { hmacSum  :: Base64
  , hmacData :: ByteString
  }

$(deriveJSON (drop 4) ''Hmac)

data Csrf a = Csrf
  { csrfKey  :: Base64
  , csrfData :: a
  }

$(deriveJSON (drop 4) ''Csrf)

cookiePrefix :: Text
cookiePrefix = "s="

cookiePrefixBytes :: ByteString
cookiePrefixBytes = encodeUtf8 cookiePrefix

getCookie :: (FromJSON a) => [Word8] -> WS.ActionM (Maybe a)
getCookie appKey = do
  r <- WS.request
  let
  { hs =
      map (BS.drop $ BS.length cookiePrefixBytes)
      $ filter (BS.startsWith cookiePrefixBytes)
      $ filter ((== "Cookie") . fst)
      $ requestHeaders r
  }
  return $ listToMaybe $ do
    hmac <- catMaybes $ map decodeJSON hs
    guard $ hmac_sha1 appKey (hmacData hmac) == hmacSum hmac
    Ae.decode $ BSL.pack $ map fromIntegral $ hmacData hmac

makeCookie :: (ToJSON a) => [Octet] -> a -> TL.Text
makeCookie appKey dat =
  fromString $ show $ Hmac (hmac_sha1 appKey hmacData) hmacData
  where
    hmacData = BSL.unpack $ Ae.encode dat

setCookie :: TL.Text -> WS.ActionM ()
setCookie = WS.header "Set-Cookie"

type ActionM' a = ErrorT Status WS.ActionM a

catcher :: (ToJSON a) => ActionM' a -> WS.ActionM ()
catcher m = runErrorT m >>= either WS.status WS.json

require :: Bool -> ActionM' ()
require c = when (not c) $ throwError status400

requireJust :: Maybe a -> ActionM' a
requireJust = maybe (throwError status400) return

randomKey :: Int -> IO [Word8]
randomKey n = mapM (const $ getStdRandom random) [1 .. n]

type RvCtx a e s m b = (Monad m, FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)

type RunVorple a e s m b =
  -- Options
  Options
  -- The initial environment
  -> e
  -- The default session state
  -> s
  -- The request handler
  -> (a -> Vorple e s m b)
  -- The application
  -> IO Application

runVorple
  :: forall a e s m b. RvCtx a e s m b
  -- The runner for the inner monad
  => (m ((Either Status b, String), s) -> IO ((Either Status b, String), s))
  -- Everything else
  -> RunVorple a e s m b
runVorple runner opts env emptySession handler = WS.scottyApp $ do
  let
  { debug :: (MonadIO i) => String -> i ()
  ; debug = when (optDebug opts) . liftIO . hPutStrLn stderr
  }
  appKey <- maybe (liftIO $ randomKey 32) return $ optAppKey opts
  WS.post "/init" $ do
    debug "Got request for /init"
    catcher $ do
      cookie <- lift $ getCookie appKey :: ActionM' (Maybe (Csrf a))
      require $ isNothing cookie
      csrfKey <- liftIO $ getStdRandom random
      lift $ setCookie $ makeCookie appKey $ Csrf csrfKey emptySession
  WS.post "/" $ do
    debug "Got request for /"
    catcher $ do
      debug "Inside the catcher"
      lift WS.body >>= debug . map (chr . fromIntegral) . BSL.unpack
      input <- lift WS.jsonData
      debug "Got JSON data"
      cookie <- lift (getCookie appKey) >>= requireJust
      debug "Got cookie"
      require $ csrfKey input == csrfKey cookie
      debug "CSRF key matches"
      let session = csrfData cookie
      let error = getv $ handler $ csrfData input
      let writer = runErrorT error
      let reader = runWriterT writer
      let state = runReaderT reader env
      let optReader = getOptionsT $ runStateT state session
      let inner = runReaderT optReader opts
      ((response, log), nextSession) <- liftIO $ runner inner
      debug "Ran request handler"
      liftIO $ hPutStr stderr log
      when (session /= nextSession)
        $ lift $ setCookie $ makeCookie appKey $ Csrf (csrfKey cookie) nextSession
      debug "About to return response"
      ErrorT $ return response

runVorpleIO :: RvCtx a e s IO b => RunVorple a e s IO b
runVorpleIO = runVorple id

runVorpleIdentity :: RvCtx a e s Identity b => RunVorple a e s Identity b
runVorpleIdentity = runVorple $ return . runIdentity

