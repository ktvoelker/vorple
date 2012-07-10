
module Types.Result where

import Data.Aeson.TH

data Result =
  LoggedIn |
  NotLoggedIn |
  Echo
  { number :: Int
  }

$(deriveJSON id ''Result)

