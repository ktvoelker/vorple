
module Vorple.Unsafe
  ( MonadAction(..)
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Web.Scotty

class (Monad m) => MonadAction m where
  liftActionM :: ActionM a -> m a

instance MonadAction ActionM where
  liftActionM = id

instance (MonadAction m) => MonadAction (ReaderT e m) where
  liftActionM = lift . liftActionM

instance (Error e, MonadAction m) => MonadAction (ErrorT e m) where
  liftActionM = lift . liftActionM

