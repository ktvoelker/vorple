
module Web.Vorple.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status())

import Web.Vorple.Text

data LogLevel =
    Critical
  | Error
  | Warning
  | Info
  | Debug
  | VorpleDebug
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Options = Options
  -- Enable internal debug logging
  { optLogLevel :: LogLevel
  -- The secret application key
  , optAppKey   :: Maybe [Word8]
  } deriving (Eq, Ord, Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optLogLevel = VorpleDebug
  , optAppKey   = Nothing
  }

newtype OptionsT m a = OptionsT { getOptionsT :: ReaderT Options m a }

newtype Vorple e s m a = Vorple
  { getVorple :: ErrorT Status
            (WriterT ByteString
            (OptionsT
            (ReaderT e
            (StateT s m)))) a }

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

