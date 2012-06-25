
module Handlers where

import Types
import Vorple

munge = (jsonData :: Vorple Env [Int]) >>= return . map (* 2) . drop 1

