
module Web.Vorple.Util where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status(), status500)

import Web.Vorple.Text
import Web.Vorple.Types

mapOptionsT f = OptionsT . mapReaderT f . getOptionsT

mapInternal f =
  Internal
  . (mapErrorT . mapWriterT . mapOptionsT . mapReaderT) f
  . getInternal

liftInternal :: (Monad m) => Internal e m a -> Vorple e s m a
liftInternal = Vorple . getInternal . mapInternal lift

runVorpleInternal :: (Monad m) => Vorple e s m a -> s -> Internal e m (a, s)
runVorpleInternal m s = mapInternal f $ Internal $ getVorple m
  where
    f m = do
      ((result, log), state) <- runStateT m s
      return $ case result of
        Left err -> (Left err, log)
        Right result -> (Right (result, state), log)

runInternal
  :: (Monad m)
  => Internal e m a
  -> Options
  -> e
  -> m (Either Status a, ByteString)
runInternal internal opts env = inner
  where
    error = getInternal internal
    writer = runErrorT error
    optReader = getOptionsT $ runWriterT writer
    reader = runReaderT optReader opts
    inner = runReaderT reader env

