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
import Control.Monad (join)
import Options.Applicative

main :: IO ()
main = join $ execParser $ info (helper <*> (console <$> parseConsoleOptions)) $ fullDesc `mappend` progDesc "coda-console"
