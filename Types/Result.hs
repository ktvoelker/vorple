
{-# LANGUAGE TemplateHaskell #-}
module Types.Result where

import Data.Aeson.TH

data Result =
  LoggedIn |
  Echo
  { number :: Int
  }

$(deriveJSON id ''Result)

