
module Database.VorPg.Sql where

import Control.Monad.State
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder

import Database.VorPg.Types

class ShowSql a where
  showsSql :: a -> Builder -> Builder

showSql :: (ShowSql a) => a -> Text
showSql = toLazyText . flip showsSql ""

s :: State Builder () -> Builder -> Builder
s = execState

instance ShowSql Builder where
  showsSql = mappend

add :: (ShowSql a) => a -> State Builder ()
add = modify . showsSql

lit :: Builder -> State Builder ()
lit = add

markFirst :: [a] -> [(Bool, a)]
markFirst [] = []
markFirst (x : xs) = (True, x) : map (False, ) xs

instance ShowSql Stmt where
  showsSql (Insert table cols rel) = s $ do
    lit "INSERT INTO "
    add table
    lit "("
    flip mapM_ (markFirst cols) $ \(first, col) -> do
      when (not first) $ lit ", "
      add col
    lit ")"
    add rel
    lit ";"
  showsSql (Update table changes restrict) = s $ do
    lit "UPDATE "
    add table
    lit "SET "
    flip mapM_ (markFirst changes) $ \(first, (col, val)) -> do
      when (not first) $ lit ", "
      add col
      lit " = "
      add val
    lit " WHERE "
    add restrict
    lit ";"
  showsSql (Delete table restrict) = s $ do
    lit "DELETE FROM "
    add table
    lit " WHERE "
    add restrict
    lit ";"
  -- TODO SELECT

instance ShowSql Name where
  showsSql = showsSql . fromString . getName

-- TODO
instance ShowSql Rel where
  showsSql _ = ("" `mappend`)

-- TODO
instance ShowSql Val where
  showsSql _ = ("" `mappend`)

