
module Web.Vorple.Log
  ( note
  , say
  , debug
  , info
  , warn
  , err
  , crit
  ) where

import Control.Monad.Writer
import Data.Either
import Data.List
import Language.Haskell.TH

import Web.Vorple.Text
import Web.Vorple.Types

encoders :: [(Char, Name)]
encoders =
  [ ('b', 'id)
  , ('s', 'encodeUtf8)
  , ('j', 'encodeJSON)
  , ('p', 'encodeShowable)
  ]

encodeShowable :: (Show a) => a -> ByteString
encodeShowable = encodeUtf8 . packString . show

parseControls :: String -> [Either Name String]
parseControls [] = [Right "\n"]
parseControls ('%' : []) = [Right "%\n"]
parseControls ('%' : '%' : xs) = case spanLiterals xs of
  (ls, xs')   -> Right ('%' : ls) : parseControls xs'
parseControls ('%' : c : xs) = case find ((== c) . fst) encoders of
  Just (_, e) -> Left e : parseControls xs
  Nothing     -> case spanLiterals xs of
    (ls, xs') -> Right ('%' : c : ls) : parseControls xs'
parseControls xs = case spanLiterals xs of
  (ls, xs')   -> Right ls : parseControls xs'

spanLiterals :: String -> (String, String)
spanLiterals = span (/= '%')

note :: Name -> String -> ExpQ
note level xs = let cs = parseControls xs in do
  cs' <- mapM n cs
  let vs = map snd $ lefts cs'
  -- TODO can we use foldr here, and perhaps avoid reversing vs?
  let body = foldl f [| return () |] cs'
  let
  { body' =
    [| do
      { maxLevel <- asksOpt optLogLevel
      ; if maxLevel >= $(conE level) then $body else return ()
      }
    |]
  }
  foldl p body' $ reverse vs
  where
    -- Run a Control after another action
    f m c = [| $m >> $(g c) |]
    -- Generate the code to carry out a Control
    g (Right xs)  = [| tell $ encodeUtf8 $ packString xs |]
    g (Left (h, p)) = [| tell $ $(varE h) $(varE p) |]
    -- Generate a parameter name for an input
    n (Left h) = do
      name <- newName "p"
      return $ Left (h, name)
    n (Right xs) = return $ Right xs
    -- Wrap with a lambda
    p body name = lamE [varP name] body

say :: String -> ExpQ
say   = note 'VorpleDebug

-- |Make a function that logs at 'Debug' using the given format string
debug :: String -> ExpQ
debug = note 'Debug

-- |Make a function that logs at 'Info' using the given format string
info :: String -> ExpQ
info  = note 'Info

-- |Make a function that logs at 'Warning' using the given format string
warn :: String -> ExpQ
warn  = note 'Warning

-- |Make a function that logs at 'Error' using the given format string
err :: String -> ExpQ
err   = note 'Error

-- |Make a function that logs at 'Critical' using the given format string
crit :: String -> ExpQ
crit  = note 'Critical

