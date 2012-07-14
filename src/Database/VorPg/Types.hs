
module Database.VorPg.Types where

import GHC.Exts (IsString(..))

newtype Name = Name { getName :: String }
  deriving (Eq, Ord, Read, Show)

instance IsString Name where
  fromString = Name

data Stmt =
    Select Rel
  | Insert Name [Name] Rel
  | Update Name [(Name, Val)] Val
  | Delete Name Val
  deriving (Eq, Ord, Read, Show)

data Join = Inner | Outer
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Dir = Asc | Desc
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Rel =
    RLiteral [[Val]] (Maybe Name)
  | RTable   Name (Maybe Name)
  | RFilter  Rel Val
  | RJoin    Join Rel Val Rel Join
  | RGroup   Rel [Val]
  | ROrder   Rel [(Val, Dir)]
  | RUnion   Rel Rel
  | RProject Rel [(Name, Val)]
  deriving (Eq, Ord, Read, Show)

data Val =
    VInteger Integer
  | VString  String
  | VBool    Bool
  | VNull
  | VField   Name Name
  | VApply   Func [Val]
  deriving (Eq, Ord, Read, Show)

data Func =
    FPlus
  | FTimes
  | FMinus
  | FNegate
  | FAbs
  | FSignum
  | FDivide
  | FAnd
  | FOr
  | FNot
  | FEqual
  | FNotEqual
  | FLess
  | FGreater
  | FLessEq
  | FGreaterEq
  | FIsNull
  | FNotNull
  | FCount
  | FSum
  | FMean
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

