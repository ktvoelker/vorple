
module Database.VorPg.Lang.Val where

import Data.Ratio (numerator, denominator)
import GHC.Exts (IsString(..))

import Database.VorPg.Types

nullary :: Func -> Val
nullary f = VApply f []

unary :: Func -> Val -> Val
unary f x = VApply f [x]

binary :: Func -> Val -> Val -> Val
binary f x y = VApply f [x, y]

instance Num Val where
  (+)         = binary FPlus
  (*)         = binary FTimes
  (-)         = binary FMinus
  negate      = unary FNegate
  abs         = unary FAbs
  signum      = unary FSignum
  fromInteger = VInteger

instance Fractional Val where
  (/)            = binary FDivide
  recip          = binary FDivide (VInteger 1)
  fromRational r =
    binary FDivide (fromInteger $ numerator r) (fromInteger $ denominator r)

instance IsString Val where
  fromString = VString

true :: Val
true = VBool True

false :: Val
false = VBool False

none :: Val
none = VNull

(#) :: Name -> Name -> Val
(#) = VField

infix 9 #

(.&&.) :: Val -> Val -> Val
(.&&.) = binary FAnd

infixr 3 .&&.

(.||.) :: Val -> Val -> Val
(.||.) = binary FOr

infixr 2 .||.

anti :: Val -> Val
anti = unary FNot

(.=.), (.<>.), (.<.), (.>.), (.<=.), (.>=.) :: Val -> Val -> Val
(.=.)  = binary FEqual
(.<>.) = binary FNotEqual
(.<.)  = binary FLess
(.>.)  = binary FGreater
(.<=.) = binary FLessEq
(.>=.) = binary FGreaterEq
infix 4 .=., .<>., .<., .>., .<=., .>=.

isNull :: Val -> Val
isNull = unary FIsNull

notNull :: Val -> Val
notNull = unary FNotNull

count :: Val
count = nullary FCount

sum, mean :: Val -> Val
sum = unary FSum
mean = unary FMean

