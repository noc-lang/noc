module Language.Noc.Runtime.Internal where

----------------------- Modules --------------------------------------------------
import Language.Noc.Syntax.AST (Expr, REPLInput(DeclInput,ExprInput))
import Data.HashMap
import Control.Monad.Error (MonadError)
import Control.Monad.Except (Except)
import Control.Monad.RWS (RWST,MonadRWS)
import qualified Data.Map as Map

-- Datatype exceptions
--data EvalError = ZeroDivisionError | NegativeLogError | EmptyStackError deriving Show
--type Env = Map String Expr

--newtype Eval a = Eval { unEval :: RWST Env () Expr (Except EvalError) a } deriving (Functor, Applicative, Monad, MonadRWS Env () Expr, MonadError EvalError)

data Value = FloatVal Double | StringVal String deriving Show
type Values = [Value]

