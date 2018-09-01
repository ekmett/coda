module Main where

import Control.Monad
import Data.Default.Class
import Data.Foldable
import Options.Applicative
import System.IO

import Console
import Console.Options
import Console.Pretty
import Version

consoleCommand, versionCommand :: Parser (IO ())
consoleCommand = console <$> parseConsoleOptions
versionCommand = pure $ putStrLn version

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
