{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Server.Options
  ( ServerOptions(..)
  , HasServerOptions(..)
  , parseServerOptions
  ) where

import Control.Lens
import Data.Data
import Data.Default
import Options.Applicative as Options

-- | Options for @coda server@
data ServerOptions = ServerOptions
  { _serverOptionsDebug :: !Bool
  , _serverOptionsLog   :: !(Maybe FilePath)
  } deriving (Eq,Ord,Show,Read,Data)

instance Default ServerOptions where
  def = ServerOptions False Nothing

-- | Parse @coda server@ options
parseServerOptions :: Options.Parser ServerOptions
parseServerOptions = ServerOptions
  <$> switch (long "debug" <> help "enable debugging")
  <*> optional (strOption (long "log" <> short 'l' <> help "log file" <> metavar "FILE" <> action "file"))

makeClassy ''ServerOptions
