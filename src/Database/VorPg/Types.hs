
module Database.VorPg.Types where

newtype Name = Name String deriving (Eq, Ord, Read, Show)

data Stmt =
    Select Rel
  | Insert Name [Name] Rel
  | Update Name [(Name, Val)] Val
  | Delete Name Val
  deriving (Eq, Ord, Read, Show)

data Rel =
    RLiteral [[Val]]
  | RTable   Name
  | RFilter  Rel Val
  | RJoin    Bool Rel Val Rel Bool
  | RGroup   Rel [Val]
  | ROrder   Rel [(Val, Bool)]
  | RUnion   Rel
  | RProject Rel [Val]
  deriving (Eq, Ord, Read, Show)

data Val =
    VInteger Integer
  | VString  String
  | VField   Name Name
  | VApply   Func [Val]
  deriving (Eq, Ord, Read, Show)

data Func =
    FPlus
  | FMinus
  | FSum
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

