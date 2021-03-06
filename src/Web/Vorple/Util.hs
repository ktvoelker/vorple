
module Web.Vorple.Util where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Web.Vorple.Text
import Web.Vorple.Types

mapVorple
  :: (  StateT s m (Either HttpStatus a, ByteString)
     -> StateT t n (Either HttpStatus b, ByteString)
     )
  -> Vorple e s m a
  -> Vorple e t n b
mapVorple f =
  Vorple
  . (mapErrorT . mapWriterT . mapReaderT) f
  . getVorple

mapInternal
  :: (Monad m, Monad n)
  => (m (Either HttpStatus a, ByteString) -> n (Either HttpStatus b, ByteString))
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
  -> m (Either HttpStatus a, ByteString)
runInternal internal opts env = inner
  where
    error = getVorple internal
    writer = runErrorT error
    reader = runWriterT writer
    inner = flip evalStateT () $ runReaderT reader (env, opts)

