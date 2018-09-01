module Algebra.Ordered
  ( OrderedMonoid
  ) where

import Relative.Delta.Type
import Data.Monoid

--------------------------------------------------------------------------------
-- Ordered monoids
--------------------------------------------------------------------------------

-- |
-- An <https://en.wikipedia.org/wiki/Ordered_semigroup ordered monoid>.
--
-- @x '<=' y@ implies @z '<>' x '<=' z '<>' y@ and @x '<>' z '<=' y' <>' z@
class (Ord t, Monoid t) => OrderedMonoid t

instance OrderedMonoid Delta
instance OrderedMonoid Any
instance OrderedMonoid All
instance OrderedMonoid a => OrderedMonoid (Dual a)
instance Ord a => OrderedMonoid [a]
instance Ord a => OrderedMonoid (First a)
instance Ord a => OrderedMonoid (Last a)
instance OrderedMonoid a => OrderedMonoid (Maybe a)
instance OrderedMonoid ()
instance (OrderedMonoid a, OrderedMonoid b) => OrderedMonoid (a, b)

-- TODO:
-- instance (Ord a, Bounded a) => OrderedMonoid (Min a)
-- instance (Ord a, Bounded a) => OrderedMonoid (Max a)
-- instance Ord a => OrderedSemigroup (NonEmpty a)




