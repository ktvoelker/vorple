
module Main where

import Init

main :: IO ()
main = run $ \() -> return ("Hello, world!" :: String)

