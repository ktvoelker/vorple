
module DB where

import Control.Monad.Reader
import qualified Database.HDBC as D
import Database.HDBC.PostgreSQL (Connection)
import Network.HTTP.Types (status400)

import Types
import Vorple

a1 f = (f .)

a2 f = ((f .) .)

a3 f = (((f .) .) .)

f1 :: (a -> IO b) -> a -> Vorple Env b
f1 = a1 liftIO

f2 :: (a -> b -> IO c) -> a -> b -> Vorple Env c
f2 = a2 liftIO

withConn0 :: (Connection -> IO a) -> Vorple Env a
withConn0 f = asks conn >>= liftIO . f

withConn1 :: (Connection -> a -> IO b) -> a -> Vorple Env b
withConn1 f a = asks conn >>= liftIO . flip f a

withConn2 :: (Connection -> a -> b -> IO c) -> a -> b -> Vorple Env c
withConn2 f a b = asks conn >>= liftIO . flip (flip f a) b

run = withConn2 D.run

prepare = withConn1 D.prepare

execute = f2 D.execute

fetchRow = f1 D.fetchRow

mustFetchRow = fetchRow >=> \r -> case r of
  Nothing -> throwError status400
  Just r  -> return r

