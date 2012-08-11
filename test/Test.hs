
module Main where

import Util

data Rec1
  = Rec1a { ax :: Int, ay :: Int }
  | Rec1b { bx :: Int, by :: Int }
  deriving (Eq, Ord, Read, Show)

$(deriveJSON id ''Rec1)

appEcho = vorple defaultOptions () () $ \r -> return (r :: Rec1)

testEcho1 = sessionTest "testEcho1" appEcho $ do
  let b1 = Rec1a 0 1
  r1 <- rj $ Csrf "" b1
  assertStatus 200 r1
  assertNoCookie r1
  k1 <- assertJsonBody b1 r1
  let b2 = Rec1a 1 0
  r2 <- rj $ Csrf "" b2
  assertStatus 200 r2
  assertNoCookie r2
  k2 <- assertJsonBody b2 r2
  assertBool "CSRF keys should not be equal" $ k1 /= k2

data Cmd
  = CmdGet
  | CmdPut Rec1
  deriving (Eq, Ord, Read, Show)

$(deriveJSON id ''Cmd)

initState = Rec1a 0 0

appState = vorple defaultOptions () initState $ \r -> case r of
  CmdGet   -> get
  CmdPut x -> do
    prev <- get
    put x
    $(debug "put %j") x
    return prev

testState1 = sessionTest "testState1" appState $ do
  -- get initial state
  r1 <- rj $ Csrf "" CmdGet
  assertStatus 200 r1
  k1 <- assertJsonBody initState r1
  -- set b2, returning initial state
  let b2 = Rec1b 1 2
  r2 <- rj $ Csrf k1 $ CmdPut b2
  assertStatus 200 r2
  k2 <- assertJsonBody initState r2
  assertBool "CSRF keys should not be equal" $ k1 /= k2
  c2 <- assertCookie r2
  -- set b3, returning b2
  let b3 = Rec1b 3 4
  r3 <- rjc c2 $ Csrf k2 $ CmdPut b3
  assertStatus 200 r3
  k3 <- assertJsonBody b2 r3
  assertEqual "CSRF keys" k2 k3
  c3 <- assertCookie r3
  -- get b3
  r4 <- rjc c3 $ Csrf k3 CmdGet
  assertStatus 200 r4
  k4 <- assertJsonBody b3 r4
  assertEqual "CSRF keys" k3 k4

main =
  runTests
  [ testEcho1
  , testState1
  ]

