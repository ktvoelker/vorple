
-- |Vorple is a framework for serving a JSON application over HTTP via WAI.
module Web.Vorple (
  -- * Types
    Vorple()
  -- * Running a Vorple application
  , defaultOptions
  , vorple
  , vorpleIO
  , vorpleT
  -- * Options
  , Options(..)
  , LogLevel(..)
  , MonadOptions(..)
  -- * Logging functions
  , debug
  , info
  , warn
  , err
  , crit
  -- * Re-exported functions
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
import Data.Maybe
import System.IO (stderr)
import System.Random

import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W

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

-- |Make a WAI Application from a request handler with any inner monad
vorpleT
  :: forall a b e m s. (Monad m, FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)
  => (forall x. m x -> IO x)  -- ^The runner for the inner monad
  -> Options                  -- ^Options
  -> e                        -- ^The initial environment
  -> s                        -- ^The default session state
  -> (a -> Vorple e s m b)    -- ^The request handler
  -> W.Application            -- ^The application
vorpleT runner opts env emptySession handler req = liftIO $ do
  appKey <- maybe (randomKey 32) return $ optAppKey opts
  (result, log) <- flip (flip runInternal opts) env $ do
    when (W.requestMethod req /= H.methodPost) $ throwError H.status405
    $(say "Got a POST")
    maybeInput <- liftIO $ decodeJSONSource $ W.requestBody req
    input <- maybe (throwError H.status400) return maybeInput
    $(say "Got JSON data")
    cookie <- getCookie appKey (W.requestHeaders req)
    (csrfKey, session) <- case cookie of
      Nothing -> do
        $(say "No cookie; generating CSRF key")
        csrfKey <- liftIO (randomKey 32) >>= return . encodeBase64
        return (csrfKey, emptySession)
      Just cookie -> do
        $(say "Got cookie")
        require $ csrfKey input == cCsrfKey cookie
        $(say "CSRF key matches")
        session <- requireJust $ decodeJSON $ cAppData cookie
        return (cCsrfKey cookie, session)
    (response, nextSession) <-
      mapInternal (liftIO . runner)
      $ runVorpleInternal (handler $ csrfData input) session
    $(say "Ran request handler")
    cookie <-
      if session /= nextSession
      then setCookie appKey csrfKey (encodeJSON nextSession) >>= return . Just
      else return Nothing
    $(say "About to return response")
    return (Csrf csrfKey response, cookie)
  BS.hPutStr stderr log
  let
  { (status, body, cookie) = case result of
      Left status          -> (status, encodeJSONBuilder (), Nothing)
      Right (body, cookie) -> (H.status200, encodeJSONBuilder body, cookie)
  }
  return $ W.ResponseBuilder status (maybeToList cookie) body

-- |Make a WAI Application from an IO request handler
vorpleIO
  :: (FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)
  => Options                  -- ^Options
  -> e                        -- ^The initial environment
  -> s                        -- ^The default session state
  -> (a -> Vorple e s IO b)   -- ^The request handler
  -> W.Application            -- ^The application
vorpleIO = vorpleT id

-- |Make a WAI Application from a pure request handler
vorple
  :: (FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)
  => Options                       -- ^Options
  -> e                             -- ^The initial environment
  -> s                             -- ^The default session state
  -> (a -> Vorple e s Identity b)  -- ^The request handler
  -> W.Application                 -- ^The application
vorple = vorpleT $ return . runIdentity

