--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Coda.Console
import Coda.Console.Options
import Coda.Server
import Coda.Server.Options
import Coda.Version
import Control.Lens ((<&>))
import Control.Monad (join)
import Data.Foldable
import Data.Monoid
import Options.Applicative

serverCommand, consoleCommand, commands :: Parser (IO ())
serverCommand = parseServerOptions <&> server
consoleCommand = parseConsoleOptions <&> console

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
