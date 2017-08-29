{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Coda.Server.Options
  ( ServerOptions(..)
  , HasServerOptions(..)
  , parseServerOptions
  ) where

import Control.Lens
import Data.Data
import Data.Monoid
import Options.Applicative as Options

-- | Options for @coda server@
data ServerOptions = ServerOptions
  { _serverOptionsDebug :: !Bool
  , _serverOptionsLog   :: !(Maybe FilePath)
  } deriving (Eq,Ord,Show,Read,Data)

-- | Parse @coda server@ options
parseServerOptions :: Options.Parser ServerOptions
parseServerOptions = ServerOptions
  <$> switch (long "debug" <> help "enable debugging")
  <*> optional (strOption (long "log" <> short 'l' <> help "log file" <> metavar "FILE" <> action "file"))

makeClassy ''ServerOptions
