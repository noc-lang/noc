module Language.Noc.Runtime.Internal where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map as M
import Data.Text (Text)
import Language.Noc.Syntax.AST

type Stack = [Value]

type Env = M.Map Text [Value]

------
type Eval a = RWST Env () Stack (Except EvalError) a

type DeclEval = StateT Env (Except EvalError)

------
data Value = QuoteVal Stack | FloatVal Double | WordVal String | StringVal String | PrimVal (Eval ())

data EvalError = ZeroDivisionError String | EmptyStackError String | TypeError String | NameError String deriving (Show)
