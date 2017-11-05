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

import Coda.Server
import Coda.Server.Options
import Control.Monad (join)
import Options.Applicative

main :: IO ()
main = join $ execParser $ info (helper <*> (server <$> parseServerOptions)) $ fullDesc `mappend` progDesc "coda-server"
