module Language.Noc.Runtime.Eval where

----------------------- Modules --------------------------------------------------
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST

-----------------------------------------------------------------------------------

eval :: Program -> Program
eval ast = ast 

evalREPL :: REPLInput -> REPLInput -> REPLInput
evalREPL stack ast = ast