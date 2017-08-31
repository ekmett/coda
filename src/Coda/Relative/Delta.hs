{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Relative.Delta
  ( Delta(..)
  , HasDelta(..)
  , HasMonoidalDelta
  , HasOrderedDelta
  ) where

import Data.Data
import Data.FingerTree
import Data.Hashable
import Data.Semigroup
import GHC.Generics

-- | A count of UTF-16 code-units.
--
-- This forms an (obvious) Abelian group unlike
-- the merely monoidal pairs of line and column.
--
-- It is also very compact fitting in a single 'Int'.
newtype Delta = Delta Int
  deriving (Eq, Ord, Show, Read, Data, Generic, Num)

instance Hashable Delta

instance Measured Delta Delta where
  measure = id

instance Semigroup Delta where
  (<>) = (+)

instance Monoid Delta where
  mempty = 0
  mappend = (+)

--------------------------------------------------------------------------------
-- Something that has a delta
--------------------------------------------------------------------------------

-- | Something we can measure.
class HasDelta t where
  delta :: t -> Int

instance HasDelta Delta where
  delta (Delta a) = a

instance (Measured v a, HasDelta v) => HasDelta (FingerTree v a) where
  delta = delta . measure

--------------------------------------------------------------------------------
-- Monoidal deltas
--------------------------------------------------------------------------------

-- |
-- 'delta' for this type is a monoid homomorphism
--
-- @
-- 'delta' (m '<>' n) = 'delta' m <> 'delta' n
-- @
class (Monoid t, HasDelta t) => HasMonoidalDelta t where
instance HasMonoidalDelta Delta
instance (Measured v a, HasMonoidalDelta v) => HasMonoidalDelta (FingerTree v a)

--------------------------------------------------------------------------------
-- Monotone deltas
--------------------------------------------------------------------------------

-- |
-- Requires that 'delta' is monotone
--
-- @m <= n@ implies @'delta' m <= 'delta' n@
class (Ord t, HasDelta t) => HasOrderedDelta t
instance HasOrderedDelta Delta
