
module Database.VorPg.Types where

type Name = String

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
    RLiteral [[Val]]
  | RTable   Name
  | RFilter  Rel Val
  | RJoin    Join Rel Val Rel Join
  | RGroup   Rel [Val]
  | ROrder   Rel [(Val, Dir)]
  | RUnion   Rel Rel
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

