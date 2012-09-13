
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Vorple.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status(), status500)

import Web.Vorple.Text

-- |Log levels
data LogLevel =
  -- |The application cannot recover
    Critical
  -- |The application might recover
  | Error
  -- |The handling of the current request might recover
  | Warning
  -- |An interesting fact
  | Info
  -- |A fact which is probably uninteresting
  | Debug
  -- |A fact which is only interesting to Vorple developers
  | VorpleDebug
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- |Options available when making an application
data Options = Options
  {
  -- |Logs below this level are suppressed
    optLogLevel      :: LogLevel
  -- |The secret application key (random if Nothing)
  , optAppKey        :: Maybe [Word8]
  -- |The name of the session cookie
  , optCookieName    :: Text
  -- |The path under which the session cookie should be returned by the client
  , optCookiePath    :: Maybe Text
  -- |Only allow the session cookie to be sent over a secure connection
  , optSecureCookie  :: Bool
  } deriving (Eq, Ord, Read, Show)

-- |A default set of options
defaultOptions :: Options
defaultOptions = Options
  { optLogLevel      = VorpleDebug
  , optAppKey        = Nothing
  , optCookieName    = "s"
  , optCookiePath    = Just "/"
  , optSecureCookie  = False
  }

-- |A monad transformer for a JSON-over-HTTP application
newtype Vorple e s m a = Vorple
  { getVorple :: ErrorT Status
            (WriterT ByteString
            (ReaderT (e, Options)
            (StateT s m))) a }

type Internal e m a = Vorple e () m a

data Cookie = Cookie
  { cHmacSum :: Base64
  , cCsrfKey :: Base64
  , cAppData :: ByteString
  }

$(deriveJSON (drop 1) ''Cookie)

data Csrf a = Csrf
  { csrfKey  :: Base64
  , csrfData :: a
  }

$(deriveJSON (drop 4) ''Csrf)

class MonadOptions m where
  asksOpt :: (Options -> a) -> m a

instance (Monad m) => MonadOptions (Vorple e s m) where
  asksOpt = Vorple . asks . (. snd)

instance Error Status where
  noMsg = status500

instance (Monad m) => Monad (Vorple e s m) where
  return = Vorple . return
  Vorple m >>= f = Vorple $ m >>= getVorple . f
  fail = Vorple . fail

instance MonadTrans (Vorple e s) where
  lift = Vorple . lift . lift . lift . lift

-- |With this instance, you can throw an HTTP error code to abort the request
-- and return that code to the client
instance (Monad m) => MonadError Status (Vorple e s m) where
  throwError     = Vorple . throwError
  catchError m f = Vorple $ getVorple m `catchError` (getVorple . f)

-- |This instance provides access to the environment that you specify when creating
-- the application
instance (Monad m) => MonadReader e (Vorple e s m) where
  ask     = Vorple $ asks fst
  local f = Vorple . local (\(e, o) -> (f e, o)) . getVorple

-- |This instance provides access to the client's session state, which is stored on
-- the client in a cookie.
instance (Monad m) => MonadState s (Vorple e s m) where
  get = Vorple get
  put = Vorple . put

-- |This instance provides a log which is written to standard error.
instance (Monad m) => MonadWriter ByteString (Vorple e s m) where
  writer = Vorple . writer
  tell   = Vorple . tell
  listen = Vorple . listen . getVorple
  pass   = Vorple . pass . getVorple

instance (MonadIO m) => MonadIO (Vorple e s m) where
  liftIO = Vorple . liftIO

