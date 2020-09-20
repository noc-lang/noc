module CLI.Commands where

import Options.Applicative
import Data.Semigroup ((<>))

type Filepath = String
data Command = Version | Exec Filepath | Repl

cmdFuncs :: [Parser Command]
cmdFuncs = [exec,repl,version]

------------------------------------------------
exec :: Parser Command
exec = Exec <$> strOption
    ( long "file"
    <> short 'f'
    <> metavar "FILENAME"
    <> help "Running Noc file." )

--------------------------------------------------
repl :: Parser Command
repl = flag Repl Repl (long "repl" <> short 'r' <> help "Running Noc REPL.")

--------------------------------------------------
version :: Parser Command
version = flag' Version (long "version" <> short 'v' <> help "Noc version.")