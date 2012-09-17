
module Main where

import Util

data Rec1
  = Rec1a { ax :: Int, ay :: Int }
  | Rec1b { bx :: Int, by :: Int }
  deriving (Eq, Ord, Read, Show)

$(deriveJSON id ''Rec1)

appEcho = vorple defaultOptions () () $ \r -> return (r :: Rec1)

testEcho1 = sessionTest "testEcho1" appEcho $ do
  r0 <- rj $ Csrf "" (Nothing :: Maybe Rec1)
  assertStatus 200 r0
  k0 <- assertJsonBody (Nothing :: Maybe Rec1) r0
  let b1 = Just $ Rec1a 0 1
  r1 <- rj $ Csrf k0 b1
  assertStatus 200 r1
  _ <- assertCookie "s"
  k1 <- assertJsonBody b1 r1
  let b2 = Just $ Rec1a 1 0
  r2 <- rj $ Csrf k1 b2
  assertStatus 200 r2
  _ <- assertCookie "s"
  k2 <- assertJsonBody b2 r2
  liftIO $ assertBool "CSRF keys should be equal" $ k1 == k2

data Cmd
  = CmdGet
  | CmdPut Rec1
  deriving (Eq, Ord, Read, Show)

$(deriveJSON id ''Cmd)

initState = Rec1a 0 0

handleState CmdGet = get
handleState (CmdPut x) = do
  prev <- get
  put x
  $(debug "put %j") x
  return prev

appState = vorple defaultOptions () initState handleState

appStateWithKey k =
  vorple defaultOptions { optAppKey = Just k } () initState handleState

testState1 = sessionTest "testState1" appState $ do
  r0 <- rj $ Csrf "" (Nothing :: Maybe Cmd)
  assertStatus 200 r0
  k0 <- assertJsonBody (Nothing :: Maybe Rec1) r0
  -- get initial state
  r1 <- rj $ Csrf k0 $ Just CmdGet
  assertStatus 200 r1
  k1 <- assertJsonBody (Just initState) r1
  -- set b2, returning initial state
  let b2 = Rec1b 1 2
  r2 <- rj $ Csrf k1 $ Just $ CmdPut b2
  assertStatus 200 r2
  k2 <- assertJsonBody (Just initState) r2
  liftIO $ assertBool "CSRF keys should be equal" $ k1 == k2
  _ <- assertCookie "s"
  -- set b3, returning b2
  let b3 = Rec1b 3 4
  r3 <- rj $ Csrf k2 $ Just $ CmdPut b3
  assertStatus 200 r3
  k3 <- assertJsonBody (Just b2) r3
  liftIO $ assertEqual "CSRF keys" k2 k3
  _ <- assertCookie "s"
  -- get b3
  r4 <- rj $ Csrf k3 $ Just CmdGet
  assertStatus 200 r4
  k4 <- assertJsonBody (Just b3) r4
  liftIO $ assertEqual "CSRF keys" k3 k4

testAppKey1 = multiTest "testAppKey1" $ \rules -> do
  c1 <- flip (runSession rules) (appStateWithKey [1, 2, 3, 4]) s
  c2 <- flip (runSession rules) (appStateWithKey [5, 6, 7, 8]) s
  assertBool "HMAC sums should differ" $ c1 /= c2
  where
    s = do
      r0 <- rj $ Csrf "" (Nothing :: Maybe Cmd)
      assertStatus 200 r0
      k0 <- assertJsonBody (Nothing :: Maybe Rec1) r0
      r <- rj $ Csrf k0 $ Just $ CmdPut $ Rec1a 1 1
      assertStatus 200 r
      _ <- assertJsonBody (Just initState) r
      assertCookie "s"

appStateNamed name =
  vorple defaultOptions { optCookieName = name } () initState handleState

testCookieName1 =
  let
    name = "c0"
  in sessionTest "testCookieName1" (appStateNamed $ fromString name) $ do
    r0 <- rj $ Csrf "" (Nothing :: Maybe Cmd)
    assertStatus 200 r0
    k0 <- assertJsonBody (Nothing :: Maybe Rec1) r0
    let b1 = Rec1b 0 1
    r1 <- rj $ Csrf k0 $ Just $ CmdPut b1
    assertStatus 200 r1
    k1 <- assertJsonBody (Just initState) r1
    _ <- assertCookie name
    r2 <- rj $ Csrf k1 $ Just CmdGet
    assertStatus 200 r2
    _ <- assertJsonBody (Just b1) r2
    _ <- assertCookie name
    return ()

main =
  runTests
  [ testEcho1
  , testState1
  , testAppKey1
  , testCookieName1
  ]

