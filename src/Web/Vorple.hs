
module Web.Vorple
  ( RvCtx
  , RunVorple
  , Vorple()
  , Options(..)
  , defaultOptions
  , runVorple
  , runVorpleIO
  , runVorpleIdentity
  , throwError
  , ask
  , asks
  , get
  , put
  , modify
  , liftIO
  , debug
  , info
  , warn
  , err
  , crit
  ) where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Random

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W

import Web.Vorple.Class
import Web.Vorple.Cookie
import Web.Vorple.Log
import Web.Vorple.Text
import Web.Vorple.Types
import Web.Vorple.Util

require :: (MonadError H.Status m) => Bool -> m ()
require c = when (not c) $ throwError H.status400

requireJust :: (MonadError H.Status m) => Maybe a -> m a
requireJust = maybe (throwError H.status400) return

randomKey :: Int -> IO [Word8]
randomKey n = mapM (const $ getStdRandom random) [1 .. n]

runInternalAction
  :: forall a e. (ToJSON a)
  => Options
  -> e
  -> Internal e IO (a, Maybe ByteString)
  -> IO W.Response
runInternalAction opts env internal = do
  (result, log) <- runInternal internal opts env
  BS.hPutStr stderr log
  let
  { (status, body, cookie) = case result of
      Left status          -> (status, encodeJSONBuilder (), Nothing)
      Right (body, cookie) -> (H.status200, encodeJSONBuilder body, cookie)
  }
  let headers = maybe [] (flip setCookie []) cookie
  return $ W.ResponseBuilder status headers body

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
  -> W.Application

runVorple
  :: forall a e s m b. RvCtx a e s m b
  -- The runner for the inner monad
  => (m (Either H.Status (b, s), ByteString) -> IO (Either H.Status (b, s), ByteString))
  -- Everything else
  -> RunVorple a e s m b
runVorple runner opts env emptySession handler req = do
  appKey <- maybe (liftIO $ randomKey 32) return $ optAppKey opts
  lift $ case W.pathInfo req of
    ["init"] -> runInternalAction opts env $ do
      $(say "Got request for /init")
      cookie <- getCookie appKey $ W.requestHeaders req
      require $ isNothing cookie
      csrfKey <- liftIO (randomKey 32) >>= return . encodeBase64
      return ((), Just $ makeCookie appKey csrfKey emptySession)
    [] -> runInternalAction opts env $ do
      $(say "Got request for /")
      maybeInput <- liftIO $ decodeJSONSource $ W.requestBody req
      input <- maybe (throwError H.status400) return maybeInput
      $(say "Got JSON data")
      cookie <- getCookie appKey (W.requestHeaders req) >>= requireJust
      $(say "Got cookie")
      require $ csrfKey input == cCsrfKey cookie
      $(say "CSRF key matches")
      session <- requireJust $ decodeJSON $ cAppData cookie
      (response, nextSession) <-
        mapInternal (liftIO . runner)
        $ runVorpleInternal (handler $ csrfData input) session
      $(say "Ran request handler")
      let
      { cookieBytes =
        if session /= nextSession
        then Just $ makeCookie appKey (cCsrfKey cookie) $ encodeJSON nextSession
        else Nothing
      }
      $(say "About to return response")
      return (response, cookieBytes)

runVorpleIO :: RvCtx a e s IO b => RunVorple a e s IO b
runVorpleIO = runVorple id

runVorpleIdentity :: RvCtx a e s Identity b => RunVorple a e s Identity b
runVorpleIdentity = runVorple $ return . runIdentity

