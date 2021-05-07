module Language.Noc.Runtime.Eval where

-------------- Modules -----------------------

import Control.Monad.RWS
import Control.Monad.Reader (ask)
import qualified Data.Map as M (empty, fromList, union)
import qualified Data.Text as T (pack)
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST

----------------- Prelude --------------------

prelude :: Env
prelude = M.empty

readValue :: Atom -> Value
readValue (FloatAtom x) = FloatVal x
readValue (WordAtom x) = WordVal x
readValue (StringAtom x) = StringVal x
readValue (QuoteAtom l) = QuoteVal $ map readValue l

eval :: Expr -> Eval ()
eval expr = do
  stack <- get
  let values = map readValue expr
  put $ stack <> values
  return ()

evalFunc :: Declaration -> DeclEval ()
evalFunc func = do
  env <- get
  let funcName = declName func
  let values = map readValue (declVal func)
  put $ env `M.union` (M.fromList [(T.pack funcName, values)])
  return ()
