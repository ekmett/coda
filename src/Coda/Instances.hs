{-# options_ghc -Wno-orphans #-}
module Coda.Instances where

import Control.Applicative
import Data.Void
import Data.Aeson

instance ToJSON Void where
  toJSON = absurd
  toEncoding = absurd

instance FromJSON Void where
  parseJSON _ = empty
