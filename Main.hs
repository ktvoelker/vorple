
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.FastCGI as FastCGI

import Control.Monad
import System.Environment
import Web.Scotty

main = do
  args <- getArgs
  let
  { runner = case args of
      ["fcgi"] -> scottyApp >=> FastCGI.run
      ["warp"] -> scotty 3000
      _ -> const $ putStrLn "Usage: Main [fcgi|warp]"
  }
  runner $ do
    get "/" $ do
      html "<html><body><h1>Hello, world!</h1></body></html>"

