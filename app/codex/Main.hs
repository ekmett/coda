
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Control.Monad (join)
import Data.Foldable
import Options.Applicative

import Console
import Console.Options
import Server
import Server.Options
import Version

serverCommand, consoleCommand, commands :: Parser (IO ())
serverCommand = server <$> parseServerOptions
consoleCommand = console <$> parseConsoleOptions

commands = subparser $ fold
  [ command "repl"    $ info (helper <*> consoleCommand) $
      progDesc "Start a REPL"
  , command "server"  $ info (helper <*> serverCommand) $
      progDesc "Begin a language server session"
  , command "version" $ info (putStrLn version <$ helper) $
      progDesc "Show detailed version information"
  ]

main :: IO ()
main = join $ execParser $ info (helper <*> commands) $ fullDesc <> progDesc "coda"
