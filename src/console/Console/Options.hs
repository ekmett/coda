{-# language DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
module Console.Options where

import Control.Lens
import Data.Default.Class
import GHC.Generics
import Options.Applicative as Options

import Console.Pretty

data ConsoleOptions = ConsoleOptions
  { _consoleFancyOptions :: FancyOptions
  , _consoleOptionsNoHeading :: Bool
  }
  deriving (Show,Generic)

instance Default ConsoleOptions where
  def = ConsoleOptions def False

parseConsoleOptions :: Options.Parser ConsoleOptions
parseConsoleOptions = ConsoleOptions <$> parseFancyOptions <*> pure False

makeClassy ''ConsoleOptions

instance HasFancyOptions ConsoleOptions where
  fancyOptions = consoleFancyOptions
