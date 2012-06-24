
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.FastCGI as FastCGI

import Control.Monad
import System.Environment
import Web.Scotty

import Vorple

main = do
  args <- getArgs
  let
  { runner = case args of
      ["warp"] -> scotty 3000
      _ -> scottyApp >=> FastCGI.run
  }
  runner $ do
    get "/" $ do
      html "<html><body><h1>Hello, world!</h1></body></html>"

