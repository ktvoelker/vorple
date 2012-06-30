
{-# LANGUAGE OverloadedStrings #-}
module Listen where

import Web.Scotty

import Server
import Types
import Vorple

main = runServer $ return ()

