module Main where

import Coda.Server.Options
import Data.Foldable
import Data.Monoid
import Options.Applicative

data Command
  = CommandServer ServerOptions
  | CommandRepl
  deriving Show

server, repl, commands :: Parser Command
server = CommandServer <$> parseServerOptions
repl   = pure CommandRepl

commands = subparser $ fold
  [ command "server" $ info (helper <*> server) (progDesc "Begin a language server session")
  , command "repl"   $ info (helper <*> repl) (progDesc "Start a REPL")
  ]

main :: IO ()
main = do
  cmd <- execParser $ info (helper <*> commands) $ fullDesc <> progDesc "coda"
  print cmd
