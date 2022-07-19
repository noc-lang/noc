module Command.CLI where

import Options.Applicative hiding (ParseError)
import Data.Version (showVersion)
import Command.Commands
import qualified Paths_noc as PN (version)
import Language.Noc.Compiler.Bytecode
import Language.Noc.Compiler.Serialize (serializeBytecode)
import Language.Noc.Syntax.AST
import Language.Noc.Resolution.Imports (parseImports)

opts :: ParserInfo Command
opts = info (helper <*> cmd) (fullDesc <> header "noc - A user-friendly concatenative language.")

run :: Command -> IO ()
run Version = putStrLn $ "Noc version " <> (showVersion PN.version)
run (Exec (path : _)) = do
  ast <- parseNocFile path
  case ast of
    (Left err) -> print err
    (Right succ) -> do
      imp <- parseImports [succ] (decls succ)
      case imp of
        (Left err) -> print err
        (Right funcs) -> do
          bytecode <- genBytecode funcs (Bytecode {sym = [], prim = [], constant = [], doc = [], opcodes = [], nb_functions = 0})
          serializeBytecode ".bytecode.o" bytecode

cmd :: Parser Command
cmd = foldl (<|>) empty cmdFuncs