
module Database.VorPg.Lang.Stmt
  ( RelM()
  , select
  , insert
  , update
  , Binding(..)
  , delete
  , from
  , values
  , hereTable
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

unbind :: Binding -> (Name, Val)
unbind (n := v) = (n, v)

update :: Name -> [Binding] -> Val -> Stmt
update n = Update n . map unbind

delete :: Name -> Val -> Stmt
delete = Delete

from :: Rel -> RelM () -> Rel
from r m = execState (getRelM m) r

values :: [[Val]] -> Rel
values = flip RLiteral Nothing

hereTable :: Name -> [[Val]] -> Rel
hereTable = flip RLiteral . Just

table :: Name -> Maybe Name -> Rel
table = RTable

join :: (MonadState Rel m) => Val -> Rel -> m ()
join v right = modify $ \left -> RJoin Inner left v right Inner

restrict :: (MonadState Rel m) => Val -> m ()
restrict = modify . flip RFilter

project :: (MonadState Rel m) => [Binding] -> m ()
project = modify . flip RProject . map unbind

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

