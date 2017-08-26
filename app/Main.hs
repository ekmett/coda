module Main where

import Coda.Console
import Coda.Console.Options
import Coda.Server.Options
import Coda.Version
import Data.Foldable
import Data.Monoid
import Options.Applicative

server, repl, commands :: Parser (IO ())
serverCommand = parseServerOptions <&> print
replCommand = parseConsoleOptions <&> print

commands = subparser $ fold
  [ command "repl"    $ info (helper <*> replCommand) (progDesc "Start a REPL")
  , command "server"  $ info (helper <*> serverCommand) (progDesc "Begin a language server session")
  , command "version" $ info (putStrLn version <$ helper) (progDesc "Show detailed version information")
  ]

main :: IO ()
main = join $ execParser $ info (helper <*> commands) $ fullDesc <> progDesc "coda"
