
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
-- import qualified Data.Attoparsec as At
import qualified Data.ByteString.Char8 as BSC
-- import Data.List
import Data.List.Split
-- import Data.Maybe
-- import qualified Data.Text.Lazy as T
-- import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), status401, status500)
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

instance (Show a) => Show (Csrf a) where
  showsPrec p Csrf{..} = showsPrec p csrfKey . (separator ++) . showsPrec p csrfData

instance (Read a) => Read (Csrf a) where
  readsPrec p xs = do
    (key, xs') <- readsPrec p xs
    let (ss, xs'') = splitAt (length separator) xs'
    guard $ ss == separator
    (dat, xs''') <- readsPrec p xs''
    return (Csrf key dat, xs''')

readMaybe :: (Read a) => String -> Maybe a
readMaybe xs = case reads xs of
  ((a, "") : _) -> Just a
  _ -> Nothing

type RunVorple a e s m b =
  -- The secret application key
  [Octet]
  -- The initial environmentn
  -> e
  -- The default session state
  -> s
  -- The request handler
  -> (a -> Vorple e s m b)
  -- The application
  -> IO Application

type RvCtx a e s m b = (Monad m, FromJSON a, ToJSON b, FromJSON s, ToJSON s, Eq s)

runVorple
  :: RvCtx a e s m b
  -- The runner for the inner monad
  => (m (Either Status b, s) -> IO (Either Status b, s))
  -- Everything else
  -> RunVorple a e s m b
runVorple runner appKey env emptySession handler = WS.scottyApp $ WS.post "/" $ do
  input <- WS.jsonData
  let error = getv $ handler input
  let reader = runErrorT error
  -- TODO: read the initial state from the cookie and write the result state back
  -- TODO: check the CSRF key from the cookie against one wrapping the request
  -- TODO: HMAC the session with the application key so we can trust it
  -- TODO: before HMACing, wrap the data in a CSRF key
  let state = runReaderT reader env
  (response, nextSession) <- liftIO $ runner $ runStateT state emptySession
  either WS.status WS.json response

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

