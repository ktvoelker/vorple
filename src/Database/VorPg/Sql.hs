
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

sepByM :: (Monad m) => m () -> [m ()] -> m ()
sepByM sep xs = flip mapM_ (markFirst xs) $ \(first, x) -> when (not first) sep >> x

instance ShowSql Stmt where
  showsSql (Insert table cols rel) = s $ do
    lit "INSERT INTO "
    add table
    lit "("
    sepByM (lit ", ") $ map add cols
    lit ")"
    add rel
    lit ";"
  showsSql (Update table changes restrict) = s $ do
    lit "UPDATE "
    add table
    lit "SET "
    sepByM (lit ", ") $ flip map changes $ \(col, val) -> do
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
  showsSql (Select rel) = s $ do
    add rel
    lit ";"

instance ShowSql Name where
  showsSql = showsSql . fromString . getName

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (return ())

showsSqlTable :: SqlTable -> Builder -> Builder
showsSqlTable SqlTable{..} = s $ do
  add stName
  whenJust stAlias $ \a -> do
    lit " "
    add a
showsSqlTable SqlSubQuery{..} = s $ do
  lit "("
  add stQuery
  lit ") "
  add stQAlias

data Join' = InnerJoin | LeftOuterJoin | RightOuterJoin | FullOuterJoin
  deriving (Show)

instance ShowSql Join' where
  showsSql InnerJoin      = s $ lit "INNER JOIN"
  showsSql LeftOuterJoin  = s $ lit "LEFT OUTER JOIN"
  showsSql RightOuterJoin = s $ lit "RIGHT OUTER JOIN"
  showsSql FullOuterJoin  = s $ lit "FULL OUTER JOIN"

showsSqlRel :: SqlRel -> Builder -> Builder
showsSqlRel SqlRel{..} = s $ do
    lit "SELECT "
    case srProject of
      Nothing -> lit "*"
      Just ps -> sepByM (lit ", ") $ flip map ps $ \(name, val) -> do
        add val
        lit " "
        add name
    whenJust srFirstTable $ \ft -> do
      lit " FROM "
      modify $ showsSqlTable ft
      flip mapM_ srTables $ \(join, table, val) -> do
        lit " "
        add join
        lit " "
        modify $ showsSqlTable table
        whenJust val $ \v -> do
          lit " ON "
          add v
    whenJust srRestrict $ \r -> do
      lit " WHERE "
      add r
    whenJust srGroup $ \gs -> do
      lit " GROUP BY "
      sepByM (lit ", ") $ map add gs
    whenJust srGroupRestrict $ \gr -> do
      lit " HAVING "
      add gr
    whenJust srOrderBy $ \os -> do
      lit " ORDER BY "
      sepByM (lit ", ") $ flip map os $ \(val, dir) -> add val >> add dir

data SqlTable =
  SqlTable
  { stName  :: Name
  , stAlias :: Maybe Name
  } |
  SqlSubQuery
  { stQuery :: Builder
  , stQAlias :: Name
  } deriving (Show)

data SqlRel = SqlRel
  -- first table
  { srFirstTable    :: Maybe SqlTable
  -- subsequent tables
  , srTables        :: [(Join', SqlTable, Maybe Val)]
  -- projections
  , srProject       :: Maybe [(Name, Val)]
  -- row restrictions
  , srRestrict      :: Maybe Val
  -- groups
  , srGroup         :: Maybe [Val]
  -- group restrictions
  , srGroupRestrict :: Maybe Val
  -- ordering
  , srOrderBy       :: Maybe [(Val, Dir)]
  } deriving (Show)

emptySqlRel = SqlRel
  { srFirstTable    = Nothing
  , srTables        = []
  , srProject       = Nothing
  , srRestrict      = Nothing
  , srGroup         = Nothing
  , srGroupRestrict = Nothing
  , srOrderBy       = Nothing
  }

makeSqlRel :: Rel -> State SqlRel ()
makeSqlRel _ = undefined

showsSelect :: Rel -> Builder -> Builder
showsSelect r = showsSqlRel $ execState (makeSqlRel r) emptySqlRel

instance ShowSql Rel where
  showsSql (RUnion q r) = s $ do
    lit "("
    modify $ showsSelect q
    lit ") UNION ("
    modify $ showsSelect r
    lit ")"
  showsSql r = showsSelect r

instance ShowSql Dir where
  showsSql Asc  = s $ lit "ASC"
  showsSql Desc = s $ lit "DESC"

-- TODO
instance ShowSql Val where
  showsSql _ = ("" `mappend`)

