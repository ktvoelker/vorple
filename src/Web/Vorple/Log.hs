
module Web.Vorple.Log where

import Control.Monad.Writer
import Data.List
import Data.Maybe
import Language.Haskell.TH

import qualified Data.Text.Lazy as T

import Web.Vorple.Text
import Web.Vorple.Types

data Control =
    SetLogLevel LogLevel
  | PrintWith   Name (Maybe Name)
  | Literal     String
  deriving (Eq, Ord, Show)

controls :: [(String, Control)]
controls =
  [ ("@C", SetLogLevel Critical)
  , ("@E", SetLogLevel Error)
  , ("@W", SetLogLevel Warning)
  , ("@I", SetLogLevel Info)
  , ("@D", SetLogLevel Debug)
  , ("@c", SetLogLevel Critical)
  , ("@e", SetLogLevel Error)
  , ("@w", SetLogLevel Warning)
  , ("@i", SetLogLevel Info)
  , ("@d", SetLogLevel Debug)
  , ("%b", PrintWith 'id Nothing)
  , ("%s", PrintWith 'encodeUtf8 Nothing)
  , ("%j", PrintWith 'encodeJSON Nothing)
  , ("%p", PrintWith 'encodeShowable Nothing)
  , ("%%", Literal "%")
  , ("%@", Literal "@")
  ]

encodeShowable :: (Show a) => a -> ByteString
encodeShowable = encodeUtf8 . packString . show

parseControls :: String -> [Control]
parseControls [] = []
parseControls xs@(x : xs') = case find ((`isPrefixOf` xs) . fst) controls of
  Nothing -> Literal [x] : parseControls xs'
  Just (str, ctrl) -> ctrl : parseControls (drop (length str) xs)

makeLogger :: [Control] -> ExpQ
-- TODO make all the lambdas first, generating the parameter names
-- to use later
makeLogger cs = do
  cs' <- mapM n cs
  let
  { vs = catMaybes $ flip map cs' $ \c -> case c of
      PrintWith _ v -> v
      _ -> Nothing
  }
  foldl p (foldl f [| return () |] cs') $ reverse vs
  where
    -- Run a Control after another action
    f :: ExpQ -> Control -> ExpQ
    f m c = [| $m >> $(g c) |]
    -- Generate the code to carry out a Control
    g (Literal xs)  = [| tell $ encodeUtf8 $ packString xs |]
    g (PrintWith h Nothing) = error "Impossible!"
    g (PrintWith h (Just p)) = [| tell $ $(varE h) $(varE p) |]
    g (SetLogLevel n) = [| return () |] -- TODO make this work with a local State
    -- Generate a parameter name for a PrintWith
    n (PrintWith h Nothing) = do
      name <- newName "p"
      return $ PrintWith h $ Just name
    n other = return other
    -- Wrap with a lambda
    p :: ExpQ -> Name -> ExpQ
    p body name = lamE [varP name] body

note :: String -> ExpQ
note = makeLogger . parseControls

