module Interpreter.Commands where

----------------Modules --------------------------

import Data.Semigroup ((<>))
import Options.Applicative

data Command = Version | Exec [String] | Repl | WriteStack [String]

--------------- Utils ----------------------------

cmdFuncs :: [Parser Command]
cmdFuncs = [repl, exec, version, writeStack]

------------ Command's function ------------------

exec :: Parser Command
exec = Exec <$> many (strArgument (metavar "FILENAME" <> help "Running Noc file."))

repl :: Parser Command
repl = flag Repl Repl (long "repl" <> short 'r' <> help "Running Noc REPL.")

version :: Parser Command
version = flag' Version (long "version" <> short 'v' <> help "Noc version.")

writeStack :: Parser Command
writeStack = do
  let a = WriteStack <$> many (strOption $ long "write-stack")
  let b = WriteStack <$> many (strArgument (metavar "FILENAME"))
  a <* b
