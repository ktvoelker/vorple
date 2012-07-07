
module DB where

import Control.Monad.Reader
import qualified Database.HDBC as D
import Database.HDBC.PostgreSQL (Connection)
import Network.HTTP.Types (status400)

import Types
import Web.Vorple

a1 f = (f .)

a2 f = ((f .) .)

a3 f = (((f .) .) .)

f1 :: (MonadIO m) => (a -> IO b) -> a -> Vorple Env () m b
f1 = a1 liftIO

f2 :: (MonadIO m) => (a -> b -> IO c) -> a -> b -> Vorple Env () m c
f2 = a2 liftIO

withConn0 :: (MonadIO m) => (Connection -> IO a) -> Vorple Env () m a
withConn0 f = asks conn >>= liftIO . f

withConn1 :: (MonadIO m) => (Connection -> a -> IO b) -> a -> Vorple Env () m b
withConn1 f a = asks conn >>= liftIO . flip f a

withConn2
  :: (MonadIO m)
  => (Connection -> a -> b -> IO c)
  -> a
  -> b
  -> Vorple Env () m c
withConn2 f a b = asks conn >>= liftIO . flip (flip f a) b

run :: (MonadIO m) => String -> [D.SqlValue] -> Vorple Env () m Integer
run = withConn2 D.run

prepare :: (MonadIO m) => String -> Vorple Env () m D.Statement
prepare = withConn1 D.prepare

execute :: (MonadIO m) => D.Statement -> [D.SqlValue] -> Vorple Env () m Integer
execute = f2 D.execute

fetchRow :: (MonadIO m) => D.Statement -> Vorple Env () m (Maybe [D.SqlValue])
fetchRow = f1 D.fetchRow

mustFetchRow :: (MonadIO m) => D.Statement -> Vorple Env () m [D.SqlValue]
mustFetchRow = fetchRow >=> \r -> case r of
  Nothing -> throwError status400
  Just r  -> return r

