module Main where

import Control.Monad
import Options.Applicative
import Data.Foldable
import Data.Version
import Paths_toccata

import Console.Options
import Console

consoleCommand, versionCommand :: Parser (IO ())
consoleCommand = console <$> parseConsoleOptions
versionCommand = pure $ putStrLn $ showVersion version

commands :: Parser (IO ())
commands = subparser $ fold
  [ command "repl" $ info (helper <*> consoleCommand) $ progDesc "Start a REPL"
  , command "version" $ info (helper <*> versionCommand) $ progDesc "Show version information"
  ]

main :: IO ()
main = join $ execParser $ info (helper <*> commands) $ fullDesc <> progDesc "toccata"
