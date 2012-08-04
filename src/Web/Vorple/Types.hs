
module Web.Vorple.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status())

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
  -- |Enable internal debug logging
    optLogLevel :: LogLevel
  -- |The secret application key (random if Nothing)
  , optAppKey   :: Maybe [Word8]
  } deriving (Eq, Ord, Read, Show)

-- |A default set of options
defaultOptions :: Options
defaultOptions = Options
  { optLogLevel = VorpleDebug
  , optAppKey   = Nothing
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

