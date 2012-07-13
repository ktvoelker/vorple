
module Database.VorPg.Lang.Stmt
  ( RelM()
  , select
  , insert
  , update
  , Binding(..)
  , delete
  , from
  , values
  , table
  , join
  , restrict
  , project
  , group
  , asc
  , desc
  , orderBy
  , union
  ) where

import Control.Monad.State (State(), MonadState(..), modify, execState)

import Database.VorPg.Types

newtype RelM a = RelM { getRelM :: State Rel a } deriving (Monad)

instance MonadState Rel RelM where
  get = RelM get
  put = RelM . put

select :: Rel -> Stmt
select = Select

insert :: Name -> [Name] -> Rel -> Stmt
insert = Insert

data Binding = Name := Val

infix 0 :=

update :: Name -> [Binding] -> Val -> Stmt
update n bs = Update n $ map (\(n := v) -> (n, v)) bs

delete :: Name -> Val -> Stmt
delete = Delete

from :: Rel -> RelM () -> Rel
from r m = execState (getRelM m) r

values :: [[Val]] -> Rel
values = RLiteral

table :: Name -> Rel
table = RTable

join :: (MonadState Rel m) => Val -> Rel -> m ()
join v right = modify $ \left -> RJoin Inner left v right Inner

restrict :: (MonadState Rel m) => Val -> m ()
restrict = modify . flip RFilter

project :: (MonadState Rel m) => [Val] -> m ()
project = modify . flip RProject

group :: (MonadState Rel m) => [Val] -> m ()
group = modify . flip RGroup

asc :: Val -> (Val, Dir)
asc = (, Asc)

desc :: Val -> (Val, Dir)
desc = (, Desc)

orderBy :: (MonadState Rel m) => [(Val, Dir)] -> m ()
orderBy = modify . flip ROrder

union :: Rel -> Rel -> Rel
union = RUnion

infixl 1 `union`
