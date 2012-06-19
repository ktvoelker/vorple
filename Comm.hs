
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Comm where

-- TODO replace
type M a = IO a

-- TODO replace
type Json = String

-- TODO replace
type User = Int

-- TODO replace
type Error = Int

data Method = Get | Post | Put | Delete deriving (Eq, Ord, Enum, Bounded, Show)

data UrlPart a = Static String | Variable (String -> a -> a)

data Target a = Target
  { method      :: Method
  , url         :: [UrlPart a]
  , emptyParams :: a
  }

data Handler a =
    RequireAuth   (User -> a -> Json -> Either Error Json)
  | DoesAuth      (a -> Json -> Either Error (User, Json))
  | NoRequireAuth (a -> Json -> Either Error Json)

run :: Handler a -> M ()
run (RequireAuth h) = return ()
run (DoesAuth h) = return ()
run (NoRequireAuth h) = return ()

serve :: (forall a. (Target a, Handler a)) -> M ()
serve (Target { .. }, handler) = return ()

--
-- For RequireAuth, check the cookie to find the current user, and check the
-- CSRF secret against the cookie, too.
--
-- For DoesAuth, create a cookie and initial CSRF secret for the newly
-- authenticated user.
--
-- For NoRequireAuth, no secret or cookie is required.
--
-- Note that none of this requires DB access.

serveAll :: [forall a. (Target a, Handler a)] -> M ()
serveAll = mapM_ serve
-- Note: this probably requires existential types to be useful

