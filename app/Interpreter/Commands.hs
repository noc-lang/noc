module Interpreter.Commands where

----------------------- Modules --------------------------------------------------
import Data.Semigroup ((<>))
import Options.Applicative

type Filepath = String

data Command = Version | Exec Filepath | Repl

--------------- Utils ----------------------------

cmdFuncs :: [Parser Command]
cmdFuncs = [exec, repl, version]

------------ Function commands -------------------
exec :: Parser Command
exec =
  Exec
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Running Noc file."
      )

repl :: Parser Command
repl = flag Repl Repl (long "repl" <> short 'r' <> help "Running Noc REPL.")

version :: Parser Command
version = flag' Version (long "version" <> short 'v' <> help "Noc version.")