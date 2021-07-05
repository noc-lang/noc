{-# LANGUAGE RankNTypes #-}

module Language.Noc.Runtime.Internal where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T (Text, isInfixOf, pack, unpack)
import Language.Noc.Syntax.AST

type Stack = [Value]

type Env = M.Map T.Text EnvEntry

data EnvEntry = Function (Maybe DocString, Expr) | Constant (DocString, Value)

------
type Eval a = RWST Env () Stack (ExceptT EvalError IO) a

type DeclEval = StateT Env (Except EvalError)

------
data Value = QuoteVal Expr | FloatVal Double | IntVal Integer | StringVal T.Text | CharVal Char | BoolVal Bool | PrimVal (Eval ())

data EvalError = ZeroDivisionError String | EmptyStackError String | TypeError String | NameError String | ValueError String deriving (Show, Eq)

---------- Utils ---------------

readAtom :: Value -> Atom
readAtom (FloatVal x) = FloatAtom x
readAtom (IntVal x) = IntAtom x
readAtom (StringVal x) = StringAtom $ T.unpack x
readAtom (CharVal x) = CharAtom x
readAtom (BoolVal x) = BoolAtom x
readAtom (QuoteVal l) = QuoteAtom l

readValue :: Atom -> Value
readValue (FloatAtom x) = FloatVal x
readValue (IntAtom x) = IntVal x
readValue (StringAtom x) = StringVal $ T.pack x
readValue (CharAtom x) = CharVal x
readValue (BoolAtom x) = BoolVal x
readValue (QuoteAtom l) = QuoteVal l

isChar :: Atom -> Bool
isChar x = case x of
  (CharAtom x) -> True
  _ -> False

------ format function utils ------

initSafe :: String -> String
initSafe [] = []
initSafe t = let (x : xs) = reverse t in reverse xs

isBrace :: T.Text -> Bool
isBrace x = (T.pack "{}") `T.isInfixOf` x

putAllInQuote :: Expr -> Eval ()
putAllInQuote res = do
  stack <- get
  case stack of
    [] -> push $ QuoteVal res
    _ -> do
      v <- pop
      putAllInQuote (readAtom v : res)

------ case function utils ------

isPattern :: Atom -> Bool
isPattern x = case x of
  (QuoteAtom [QuoteAtom _, QuoteAtom _]) -> True
  (QuoteAtom _) -> True
  _ -> False

eqValue :: Value -> Value -> Bool
eqValue a b =
  case (a, b) of
    (QuoteVal x, QuoteVal y) -> x == y
    (FloatVal x, FloatVal y) -> x == y
    (IntVal x, IntVal y) -> x == y
    (FloatVal x, IntVal y) -> x == (fromIntegral y)
    (IntVal x, FloatVal y) -> (fromIntegral x) == y
    (StringVal x, StringVal y) -> x == y
    (CharVal x, CharVal y) -> x == y
    (BoolVal x, BoolVal y) -> x == y
    _ -> False

-------------------------------

pop :: Eval Value
pop = do
  stack <- get
  case stack of
    [] -> throwError $ EmptyStackError "no or few elements in the stack."
    _ -> let (x : xs) = reverse stack in ((put $ reverse xs) >> return x)

-------------------------------

push :: Value -> Eval ()
push val = do
  stack <- get
  put $ stack <> [val]
  return ()

-------------------------------

evalExpr :: Expr -> Eval ()
evalExpr [] = return ()
evalExpr (x : xs) = case x of
  (WordAtom n) -> (ask >>= (evalWord n)) >> evalExpr xs
  y -> case y of
    (QuoteAtom z) -> case all isChar z && z /= [] of
      True -> (push $ StringVal $ T.pack $ foldr (\(CharAtom x) acc -> x : acc) [] z) >> evalExpr xs
      False -> (push $ readValue x) >> evalExpr xs
    _ -> (push $ readValue x) >> evalExpr xs

-------------------------------

evalWord :: String -> Env -> Eval ()
evalWord w e = do
  case M.lookup (T.pack w) e of
    (Just (Constant (_, PrimVal f))) -> f
    (Just (Function (_, expr))) -> evalExpr expr
    Nothing -> throwError (NameError (w <> " function doesn't declared or not in prelude.")) >> return ()
