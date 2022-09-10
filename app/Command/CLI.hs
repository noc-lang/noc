module Command.CLI where

import Options.Applicative hiding (ParseError)
import Data.Version (showVersion)
import Command.Commands
import qualified Paths_noc as PN (version)
import Language.Noc.Compiler.Bytecode
import Language.Noc.Compiler.Serialize (serializeBytecode)
import Language.Noc.Syntax.AST
import Language.Noc.Resolution.Imports (parseImports)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.Info (os)
import System.Process (system)

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
          bytecode <- genBytecode funcs (Bytecode {sym = [], constant = [], doc = [], opcodes = []})
          bytecode_path <- getXdgDirectory XdgCache ".bytecode"
          serializeBytecode bytecode_path bytecode
          noc_vm_path <- getXdgDirectory XdgData (if os == "mingw32" then "local/noc/noc_vm" else "noc/noc_vm")
          system $ noc_vm_path <> " " <> bytecode_path
          return ()
run _ = putStrLn "noc: no input file"

cmd :: Parser Command
cmd = foldl (<|>) empty cmdFuncs