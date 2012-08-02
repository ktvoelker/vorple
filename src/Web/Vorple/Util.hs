
module Web.Vorple.Util where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status(), status500)

import Web.Vorple.Text
import Web.Vorple.Types

mapOptionsT f = OptionsT . mapReaderT f . getOptionsT

mapStateT' f = mapStateT f'
  where
    f' (x, s) = (f x, s)

mapVorple
  :: (  StateT s m (Either Status a, ByteString)
     -> StateT t n (Either Status b, ByteString)
     )
  -> Vorple e s m a
  -> Vorple e t n b
mapVorple f =
  Vorple
  . (mapErrorT . mapWriterT . mapOptionsT . mapReaderT) f
  . getVorple

mapInternal
  :: (Monad m, Monad n)
  => (m (Either Status a, ByteString) -> n (Either Status b, ByteString))
  -> Internal e m a
  -> Internal e n b
mapInternal = mapVorple . (lift .) . (. flip evalStateT ())

liftInternal :: (Monad m) => Internal e m a -> Vorple e s m a
liftInternal = mapVorple $ lift . flip evalStateT ()

runVorpleInternal :: (Monad m) => Vorple e s m a -> s -> Internal e m (a, s)
runVorpleInternal m s = mapVorple f m
  where
    f m = do
      ((result, log), state) <- lift $ runStateT m s
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
    error = getVorple internal
    writer = runErrorT error
    optReader = getOptionsT $ runWriterT writer
    reader = runReaderT optReader opts
    inner = flip evalStateT () $ runReaderT reader env

