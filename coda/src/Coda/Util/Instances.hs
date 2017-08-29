{-# options_ghc -Wno-orphans #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Missing instances
--------------------------------------------------------------------

module Coda.Util.Instances where

import Control.Applicative
import Data.Void
import Data.Aeson

instance ToJSON Void where
  toJSON = absurd
  toEncoding = absurd

instance FromJSON Void where
  parseJSON _ = empty
