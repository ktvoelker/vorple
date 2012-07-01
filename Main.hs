
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Handlers
import Server
import Types
import Vorple

main = runServer handle

