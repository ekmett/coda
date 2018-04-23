module Main where

import Coda.Console
import Coda.Console.Options
import Coda.Console.Pretty
import Control.Monad
import Data.Default.Class
import Data.Foldable
import Data.Version
import Options.Applicative
import Paths_coda
import System.IO

consoleCommand, versionCommand :: Parser (IO ())
consoleCommand = console <$> parseConsoleOptions
versionCommand = pure $ putStrLn $ showVersion version

commands :: Parser (IO ())
commands = subparser $ fold
  [ command "repl" $ info (helper <*> consoleCommand) $ progDesc "Start a REPL"
  , command "version" $ info (helper <*> versionCommand) $ progDesc "Show version information"
  ]

main :: IO ()
main = do
  n <- fcols def stdout -- compute display columns
  let mods = columns n <> disambiguate
  join $ customExecParser (prefs mods) $ info (helper <*> commands) $ fullDesc <> progDesc "toccata"
