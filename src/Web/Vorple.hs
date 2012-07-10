
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

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as WS

import Web.Vorple.Class
import Web.Vorple.Text
import Web.Vorple.Types
import Web.Vorple.Util

asksOpt :: (Monad m) => (Options -> a) -> Vorple e s m a
asksOpt = Vorple . lift . lift . OptionsT . asks

logs :: (Monad m) => Text -> Vorple e s m ()
logs = (>> tell "\n") . tell . encodeUtf8

logp :: (Monad m, Show a) => a -> Vorple e s m ()
logp = logs . packString . show

logj :: (Monad m, ToJSON a) => a -> Vorple e s m ()
logj = (>> tell "\n") . tell . encodeJSON

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

getCookie :: (FromJSON a) => [Word8] -> WS.ActionM (Maybe a)
getCookie appKey = do
  r <- WS.request
  let
  { hs =
      map (BS.drop $ BS.length cookiePrefixBytes)
      $ filter (cookiePrefixBytes `BS.isPrefixOf`)
      $ map (BS.fromChunks . (: []) . snd)
      $ filter ((== "Cookie") . fst)
      $ requestHeaders r
  }
  return $ listToMaybe $ do
    Cookie{..} <- catMaybes $ map decodeJSON hs
    guard $ getHmacSum appKey cCsrfKey cAppData == cHmacSum
    maybeToList $ decodeJSON cAppData

makeCookie :: (ToJSON a) => [Word8] -> Base64 -> a -> Text
makeCookie appKey csrfKey appData =
  cookiePrefix
  `T.append`
  showJSON (Cookie (getHmacSum appKey csrfKey appDataBytes) csrfKey appDataBytes)
  where
    appDataBytes = encodeJSON appData

setCookie :: Text -> WS.ActionM ()
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
  => (m ((Either Status b, ByteString), s) -> IO ((Either Status b, ByteString), s))
  -- Everything else
  -> RunVorple a e s m b
runVorple runner opts env emptySession handler = WS.scottyApp $ do
  let
  { debug :: (MonadIO i) => ByteString -> i ()
  ; debug =
      when (optDebug opts)
      . liftIO
      . (>> BS.hPutStr stderr "\n")
      . BS.hPutStr stderr
  }
  appKey <- maybe (liftIO $ randomKey 32) return $ optAppKey opts
  WS.post "/init" $ do
    debug "Got request for /init"
    catcher $ do
      cookie <- lift $ getCookie appKey :: ActionM' (Maybe Cookie)
      require $ isNothing cookie
      csrfKey <- liftIO (randomKey 32) >>= return . encodeBase64
      lift $ setCookie $ makeCookie appKey csrfKey emptySession
  WS.post "/" $ do
    debug "Got request for /"
    catcher $ do
      debug "Inside the catcher"
      lift WS.body >>= debug
      input <- lift WS.jsonData
      debug "Got JSON data"
      cookie <- lift (getCookie appKey :: WS.ActionM (Maybe Cookie)) >>= requireJust
      debug "Got cookie"
      require $ csrfKey input == cCsrfKey cookie
      debug "CSRF key matches"
      session <- requireJust $ decodeJSON $ cAppData cookie
      let error = getVorple $ handler $ csrfData input
      let writer = runErrorT error
      let optReader = getOptionsT $ runWriterT writer
      let reader = runReaderT optReader opts
      let state = runReaderT reader env
      let inner = runStateT state session
      ((response, log), nextSession) <- liftIO $ runner inner
      debug "Ran request handler"
      liftIO $ BS.hPutStr stderr log
      when (session /= nextSession)
        $ lift
        $ setCookie
        $ makeCookie appKey (cCsrfKey cookie)
        $ encodeJSON nextSession
      debug "About to return response"
      ErrorT $ return response

runVorpleIO :: RvCtx a e s IO b => RunVorple a e s IO b
runVorpleIO = runVorple id

runVorpleIdentity :: RvCtx a e s Identity b => RunVorple a e s Identity b
runVorpleIdentity = runVorple $ return . runIdentity

