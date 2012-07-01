
{-# LANGUAGE TemplateHaskell #-}
module Types.Command where

import Data.Aeson.TH

data Command =
  Login
  { email    :: String
  , password :: String
  } |
  Echo
  { number   :: Int
  }

$(deriveJSON id ''Command)

