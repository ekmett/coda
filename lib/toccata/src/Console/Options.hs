{-# language DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
module Console.Options where

import Control.Lens
import Data.Default.Class
import GHC.Generics
import Options.Applicative as Options

data ConsoleOptions = ConsoleOptions
  {}
  deriving (Eq,Ord,Show,Read,Generic,Default)

parseConsoleOptions :: Options.Parser ConsoleOptions
parseConsoleOptions = pure ConsoleOptions

makeClassy ''ConsoleOptions
