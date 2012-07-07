
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
module Web.Vorple
  ( Vorple()
  , runVorple
  , runVorpleIO
  , runVorpleIdentity
  , throwError
  ) where

import Codec.Utils
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as Ae
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.HMAC
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text.Lazy as TL
import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), status400, status401, status500)
import Network.Wai (requestHeaders, Application())
import System.Random
import qualified Web.Scotty as WS

instance Error Status where
  noMsg = status500

newtype Vorple e s m a = Vorple { getv :: ErrorT Status (ReaderT e (StateT s m)) a }
  deriving (Monad)

instance (Monad m) => MonadError Status (Vorple e s m) where
  throwError = Vorple . throwError
  catchError m f = Vorple $ getv m `catchError` (getv . f)

instance (Monad m) => MonadReader e (Vorple e s m) where
  ask = Vorple ask
  local f = Vorple . local f . getv
  reader = Vorple . reader

instance (Monad m) => MonadState s (Vorple e s m) where
  get = Vorple get
  put = Vorple . put

instance (MonadIO m) => MonadIO (Vorple e s m) where
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

showsOctets :: [Octet] -> ShowS
showsOctets = (++) . BSC.unpack . B64.encode . BSW.pack

readsOctets :: ReadS [Octet]
readsOctets xs = case B64.decode $ BSC.pack xs of
  Left _   -> []
  Right bs -> [(BSW.unpack bs, [])]

separator :: String
separator = ";"

instance Show Hmac where
  showsPrec _ Hmac{..} = showsOctets hmacSum . (separator ++) . showsOctets hmacData

instance Read Hmac where
  readsPrec _ xs = case splitOn xs separator of
    [sum, dat] -> do
      (sum', []) <- readsOctets sum
      (dat', []) <- readsOctets dat
      return $ (Hmac sum' dat', [])

readMaybe :: (Read a) => String -> Maybe a
readMaybe xs = case reads xs of
  ((a, "") : _) -> Just a
  _ -> Nothing

getCookie :: (FromJSON a) => [Octet] -> WS.ActionM (Maybe a)
getCookie appKey = do
  r <- WS.request
  let hs = map (BSC.unpack . snd) $ filter ((== "Cookie") . fst) $ requestHeaders r
  return $ do
    hmac <- listToMaybe $ catMaybes $ map readMaybe hs
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

randomKey :: Int -> IO [Octet]
randomKey n = mapM (const $ getStdRandom random) [1 .. n]

type RvCtx a e s m b = (Monad m, FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)

type RunVorple a e s m b =
  -- The secret application key
  Maybe [Octet]
  -- The initial environmentn
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
  => (m (Either Status b, s) -> IO (Either Status b, s))
  -- Everything else
  -> RunVorple a e s m b
runVorple runner mAppKey env emptySession handler = WS.scottyApp $ do
  appKey <- maybe (liftIO $ randomKey 32) return mAppKey
  WS.post "/init" $ catcher $ do
    cookie <- lift $ getCookie appKey :: ActionM' (Maybe (Csrf a))
    require $ isNothing cookie
    csrfKey <- liftIO $ getStdRandom random
    lift $ setCookie $ makeCookie appKey $ Csrf csrfKey emptySession
  WS.post "/" $ catcher $ do
    input <- lift WS.jsonData
    cookie <- lift (getCookie appKey) >>= requireJust
    require $ csrfKey input == csrfKey cookie
    let session = csrfData cookie
    let error = getv $ handler $ csrfData input
    let reader = runErrorT error
    let state = runReaderT reader env
    (response, nextSession) <- liftIO $ runner $ runStateT state session
    when (session /= nextSession)
      $ lift $ setCookie $ makeCookie appKey $ Csrf (csrfKey cookie) nextSession
    ErrorT $ return response

runVorpleIO :: RvCtx a e s IO b => RunVorple a e s IO b
runVorpleIO = runVorple id

runVorpleIdentity :: RvCtx a e s Identity b => RunVorple a e s Identity b
runVorpleIdentity = runVorple $ return . runIdentity

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

