
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
  asksOpt = Vorple . lift . lift . OptionsT . asks

instance (Monad m) => MonadOptions (Internal e m) where
  asksOpt = Internal . lift . lift . OptionsT . asks

instance (Monad m) => Monad (OptionsT m) where
  return = OptionsT . return
  OptionsT m >>= f = OptionsT $ m >>= getOptionsT . f
  fail = OptionsT . fail

instance MonadTrans OptionsT where
  lift = OptionsT . lift

instance (MonadIO m) => MonadIO (OptionsT m) where
  liftIO = OptionsT . liftIO

instance (MonadReader r m) => MonadReader r (OptionsT m) where
  ask = lift ask
  local f = OptionsT . mapReaderT (local f) . getOptionsT
  reader = lift . reader

instance (MonadState s m) => MonadState s (OptionsT m) where
  get = lift get
  put = lift . put

instance Error Status where
  noMsg = status500

instance (Monad m) => Monad (Vorple e s m) where
  return = Vorple . return
  Vorple m >>= f = Vorple $ m >>= getVorple . f
  fail = Vorple . fail

instance MonadTrans (Vorple e s) where
  lift = Vorple . lift . lift . lift . lift . lift

instance (Monad m) => MonadError Status (Vorple e s m) where
  throwError     = Vorple . throwError
  catchError m f = Vorple $ getVorple m `catchError` (getVorple . f)

instance (Monad m) => MonadReader e (Vorple e s m) where
  ask     = Vorple ask
  local f = Vorple . local f . getVorple
  reader  = Vorple . reader

instance (Monad m) => MonadState s (Vorple e s m) where
  get = Vorple get
  put = Vorple . put

instance (Monad m) => MonadWriter ByteString (Vorple e s m) where
  writer = Vorple . writer
  tell   = Vorple . tell
  listen = Vorple . listen . getVorple
  pass   = Vorple . pass . getVorple

instance (MonadIO m) => MonadIO (Vorple e s m) where
  liftIO = Vorple . liftIO

instance (Monad m) => Monad (Internal e m) where
  return = Internal . return
  Internal m >>= f = Internal $ m >>= getInternal . f
  fail = Internal . fail

instance MonadTrans (Internal e) where
  lift = Internal . lift . lift . lift . lift

instance (Monad m) => MonadError Status (Internal e m) where
  throwError     = Internal . throwError
  catchError m f = Internal $ getInternal m `catchError` (getInternal . f)

instance (Monad m) => MonadReader e (Internal e m) where
  ask     = Internal ask
  local f = Internal . local f . getInternal
  reader  = Internal . reader

instance (MonadState s m) => MonadState s (Internal e m) where
  get = Internal get
  put = Internal . put

instance (Monad m) => MonadWriter ByteString (Internal e m) where
  writer = Internal . writer
  tell   = Internal . tell
  listen = Internal . listen . getInternal
  pass   = Internal . pass . getInternal

instance (MonadIO m) => MonadIO (Internal e m) where
  liftIO = Internal . liftIO

