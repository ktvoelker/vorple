
module Web.Vorple
  ( Vorple()
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
import Data.HMAC
import Data.List
import Data.List.Split
import Data.Maybe
import Network.HTTP.Types (Status(), status400, status401, status500)
import Network.Wai (requestHeaders, Application())
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Random

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as WS

import Web.Vorple.Class
import Web.Vorple.Log
import Web.Vorple.Text
import Web.Vorple.Types
import Web.Vorple.Util

cookiePrefix :: Text
cookiePrefix = "s="

cookiePrefixBytes :: ByteString
cookiePrefixBytes = encodeUtf8 cookiePrefix

getHmacSum :: [Word8] -> Base64 -> ByteString -> Base64
getHmacSum appKey csrfKey appDataBytes =
  encodeBase64
  $ hmac_sha1 appKey
  $ unpackBytes
  $ getBase64Bytes csrfKey `BS.append` appDataBytes

getCookie :: [Word8] -> Internal e WS.ActionM (Maybe Cookie)
getCookie appKey = do
  r <- lift WS.request
  let
  { hs =
      map (BS.drop (BS.length cookiePrefixBytes))
      $ filter (cookiePrefixBytes `BS.isPrefixOf`)
      $ map (BS.fromChunks . (: []) . snd)
      $ filter ((== "Cookie") . fst)
      $ requestHeaders r
  }
  $(say "POSSIBLE COOKIES:")
  mapM_ $(say "%b") hs
  let cookies = catMaybes $ map (decodeUrl >=> decodeJSON) hs :: [Cookie]
  $(say "PARSED COOKIES:")
  mapM_ $(say "%j") cookies
  let
  { cookieSums =
      map (\Cookie{..} -> getHmacSum appKey cCsrfKey cAppData == cHmacSum) cookies
  }
  $(say "COOKIE SUMS ACCEPTED:")
  mapM_ $(say "%j") cookieSums
  let cookieDataBytes = map cAppData cookies
  $(say "COOKIE APP DATA BYTES:")
  mapM $(say "%b") cookieDataBytes
  return $ listToMaybe $ do
    c@Cookie{..} <- catMaybes $ map decodeJSON hs
    guard $ getHmacSum appKey cCsrfKey cAppData == cHmacSum
    return c

makeCookie :: (ToJSON a) => [Word8] -> Base64 -> a -> Text
makeCookie appKey csrfKey appData =
  T.append cookiePrefix
  $ decodeUtf8
  $ encodeUrl
  $ encodeUtf8
  $ showJSON
  $ Cookie (getHmacSum appKey csrfKey appDataBytes) csrfKey appDataBytes
  where
    appDataBytes = encodeJSON appData

setCookie :: Text -> WS.ActionM ()
setCookie = WS.header "Set-Cookie"

require :: (MonadError Status m) => Bool -> m ()
require c = when (not c) $ throwError status400

requireJust :: (MonadError Status m) => Maybe a -> m a
requireJust = maybe (throwError status400) return

randomKey :: Int -> IO [Word8]
randomKey n = mapM (const $ getStdRandom random) [1 .. n]

runInternalAction
  :: (ToJSON a)
  => Options
  -> e
  -> Internal e WS.ActionM a
  -> WS.ActionM ()
runInternalAction opts env internal = do
  (result, log) <- runInternal internal opts env
  liftIO $ BS.hPutStr stderr log
  either WS.status WS.json result

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
  => (m (Either Status (b, s), ByteString) -> IO (Either Status (b, s), ByteString))
  -- Everything else
  -> RunVorple a e s m b
runVorple runner opts env emptySession handler = WS.scottyApp $ do
  appKey <- maybe (liftIO $ randomKey 32) return $ optAppKey opts
  WS.post "/init" $ runInternalAction opts env $ do
    $(say "Got request for /init")
    cookie <- getCookie appKey
    require $ isNothing cookie
    csrfKey <- liftIO (randomKey 32) >>= return . encodeBase64
    lift $ setCookie $ makeCookie appKey csrfKey emptySession
  WS.post "/" $ runInternalAction opts env $ do
    lift $ setCookie "foo=bar"
    $(say "Got request for /")
    lift WS.body >>= $(say "%b")
    input <- lift WS.jsonData
    $(say "Got JSON data")
    cookie <- getCookie appKey >>= requireJust
    $(say "Got cookie")
    require $ csrfKey input == cCsrfKey cookie
    $(say "CSRF key matches")
    session <- requireJust $ decodeJSON $ cAppData cookie
    (response, nextSession) <-
      mapInternal (liftIO . runner)
      $ runVorpleInternal (handler $ csrfData input) session
    $(say "Ran request handler")
    when (session /= nextSession)
      $ lift
      $ setCookie
      $ makeCookie appKey (cCsrfKey cookie)
      $ encodeJSON nextSession
    $(say "About to return response")
    return response

runVorpleIO :: RvCtx a e s IO b => RunVorple a e s IO b
runVorpleIO = runVorple id

runVorpleIdentity :: RvCtx a e s Identity b => RunVorple a e s Identity b
runVorpleIdentity = runVorple $ return . runIdentity

