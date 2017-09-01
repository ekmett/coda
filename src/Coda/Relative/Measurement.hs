---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Relative.Measurement
  ( Measurement(..)
  , measurement
  ) where

import Coda.Relative.Delta
import Data.Default
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

newtype Measurement a = Measurement { runMeasurement :: a -> Delta }

instance Contravariant Measurement where
  contramap f (Measurement g) = Measurement (g . f)

instance Divisible Measurement where
  divide f (Measurement g) (Measurement h) = Measurement $ \a -> case f a of 
    (b, c) -> g b `mappend` h c
  conquer = Measurement $ const mempty

instance Decidable Measurement where
  choose f g h = Measurement $ either (runMeasurement g) (runMeasurement h) . f
  lose f = Measurement (absurd . f)

instance Default (Measurement a) where
  def = conquer

measurement :: HasDelta a => Measurement a
measurement = Measurement delta
