
module Main where

import Test.HUnit
import Network.Wai
import Network.Wai.Test

test1 = TestLabel "test1" $ TestCase $ flip runSession testApp1 $ do
  return ()

testApp1 :: Application
-- TODO
testApp1 _ = return undefined

tests = TestList [test1]

main = runTestTT tests

