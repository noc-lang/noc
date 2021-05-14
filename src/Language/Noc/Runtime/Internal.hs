{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}

module Language.Noc.Runtime.Internal where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map as M
import Data.Text (Text,pack,unpack)
import Language.Noc.Syntax.AST

type Stack = [Value]

type Env = M.Map Text EnvEntry

data EnvEntry = Function Expr | Constant Value

------
type Eval a = RWST Env () Stack (ExceptT EvalError IO) a

type DeclEval = StateT Env (Except EvalError)

------
data Value = QuoteVal Expr | FloatVal Double | IntVal Integer | StringVal Text | BoolVal Bool | PrimVal (Eval ())

data EvalError = ZeroDivisionError String | EmptyStackError String | TypeError String | NameError String | FileNotFoundError String | MainError String | ValueError String deriving Show

--- Utils ---------------------

initN :: Int -> Stack -> Stack
initN _ [] = []
initN 0 l = l
initN n l = initN (n-1) (init l) 

readAtom :: Value -> Atom
readAtom (FloatVal x) = FloatAtom x
readAtom (IntVal x) = IntAtom x
readAtom (StringVal x) = StringAtom $ unpack x
readAtom (BoolVal x) = BoolAtom x 
readAtom (QuoteVal l) = QuoteAtom l

readValue :: Atom -> Value
readValue (FloatAtom x) = FloatVal x
readValue (IntAtom x) = IntVal x
readValue (StringAtom x) = StringVal $ pack x
readValue (BoolAtom x) = BoolVal x 
readValue (QuoteAtom l) = QuoteVal l

filterProg :: (String -> String -> Bool) -> [Declaration] -> [Declaration]
filterProg pred prog = filter (\(Declaration name expr) -> pred name "main") prog

popN :: Integer -> Eval ()
popN 0 = return ()
popN n = pop >> popN (n-1)

-------------------------------

pop :: Eval Value
pop = do
  stack <- get
  case stack of
        [] -> throwError $ EmptyStackError "no or few elements in the stack."
        _ -> let (x:xs) = reverse stack in ((put $ reverse xs) >> return x)

-------------------------------

push :: Value -> Eval ()
push val = do
  stack <- get
  put $ stack <> [val]
  return ()

-------------------------------

evalExpr :: Expr -> Eval ()
evalExpr [] = return ()
evalExpr (x:xs) = case x of
  (WordAtom n) -> (ask >>= (evalWord n)) >> evalExpr xs
  _ -> (push $ readValue x) >> evalExpr xs

-------------------------------

evalWord :: String -> Env -> Eval ()
evalWord w e = do
  case M.lookup (pack w) e of
    (Just (Constant (PrimVal f))) -> f
    (Just (Function expr)) -> evalExpr expr
    Nothing -> throwError (NameError (w <> " function doesn't declared or not in prelude.")) >> return ()