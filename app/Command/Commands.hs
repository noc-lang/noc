module Command.Commands where

import Options.Applicative

data Command = Version | Exec [String] | Repl

cmdFuncs :: [Parser Command]
cmdFuncs = [repl, exec, version]

exec :: Parser Command
exec = Exec <$> many (strArgument (metavar "FILENAME" <> help "Running Noc file."))

repl :: Parser Command
repl = flag Repl Repl (long "repl" <> short 'r' <> help "Running Noc REPL.")

version :: Parser Command
version = flag' Version (long "version" <> short 'v' <> help "Noc version.")