
module Web.Vorple.Class where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status(), status500)

import Web.Vorple.Text
import Web.Vorple.Types

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

