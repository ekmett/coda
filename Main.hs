module Main where

import Coda.Server.Options
import Data.Foldable
import Data.Monoid
import Options.Applicative

data Command = Server ServerOptions deriving Show

commands :: Parser Command
commands = subparser $ fold
  [ command "server" $ info (helper <*> (Server <$> parseServerOptions)) (progDesc "Begin a language server session")
  ]

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> commands) $ fullDesc <> progDesc "coda"
  print opts
