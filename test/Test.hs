
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
  void $ assertJsonBody b1 r1
  let b2 = Rec1a 1 0
  r2 <- rj $ Csrf "" b2
  assertStatus 200 r2
  assertNoCookie r2
  void $ assertJsonBody b2 r2

main = runTests [testEcho1]

