{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
module Coda.Server.Options
  ( ServerOptions(..)
  , HasServerOptions(..)
  , parseServerOptions
  ) where

import Control.Lens
import Data.Data
import Data.Monoid
import Options.Applicative

data ServerOptions = ServerOptions
  { _serverOptionsDebug :: !Bool
  , _serverOptionsLog   :: !(Maybe FilePath)
  } deriving (Eq,Ord,Show,Read,Data)

parseServerOptions :: Parser ServerOptions
parseServerOptions = ServerOptions
  <$> switch (long "debug" <> help "enable debugging")
  <*> optional (strOption (long "log" <> short 'l' <> help "log file" <> metavar "FILE" <> action "file"))

makeClassy ''ServerOptions
