{-# language TemplateHaskell #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Coda.Console.Options
  ( ConsoleOptions(..)
  , HasConsoleOptions(..)
  , parseConsoleOptions
  ) where

import Control.Lens
import Data.Monoid ((<>))
import Options.Applicative as Options

-- | Options for @coda repl@
data ConsoleOptions = ConsoleOptions
  { _consoleOptionsNoHeading :: Bool
  , _consoleOptionsNoUnicode :: Bool
  } deriving (Eq,Ord,Show,Read)

-- | Parse @coda repl@ options
parseConsoleOptions :: Options.Parser ConsoleOptions
parseConsoleOptions = ConsoleOptions
  <$> switch (long "no-heading" <> help "Don't show a heading at the top of the REPL")
  <*> switch (long "no-unicode" <> help "Disable code-page switching on windows")

makeClassy ''ConsoleOptions
