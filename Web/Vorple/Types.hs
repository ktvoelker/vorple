
module Web.Vorple.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status())

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

newtype Internal e m a = Internal
  { getInternal :: ErrorT Status
            (WriterT ByteString
            (OptionsT
            (ReaderT e m))) a }

newtype Vorple e s m a = Vorple
  { getVorple :: ErrorT Status
            (WriterT ByteString
            (OptionsT
            (ReaderT e
            (StateT s m)))) a }

