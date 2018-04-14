{-# language DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
module Options where

import Control.Lens
import Data.Default.Class
import GHC.Generics
import Options.Applicative as Options

data Options = Options
  {}
  deriving (Eq,Ord,Show,Read,Generic,Default)

parseOptions :: Options.Parser Options
parseOptions = pure Options

makeClassy ''Options
