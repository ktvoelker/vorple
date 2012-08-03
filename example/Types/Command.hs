
module Types.Command where

import Data.Aeson.TH

data Command =
  Login
  { email    :: String
  , password :: String
  } |
  Echo
  { number   :: Int
  } |
  Incr
  deriving (Eq, Ord, Read, Show)

$(deriveJSON id ''Command)

