{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Stuff we an measure in UTF-16 code units
---------------------------------------------------------------------------------

module Coda.Relative.Delta
  ( Delta(..)
  , HasDelta(..)
  , units
  , HasMonoidalDelta
  , HasOrderedDelta
  ) where

import Coda.FingerTree
import Data.Data
import Data.Default
import Data.Hashable
import Data.Semigroup
import GHC.Generics
import Text.Read

-- | A count of UTF-16 code-units.
--
-- This forms an (obvious) Abelian group unlike
-- the merely monoidal pairs of line and column.
--
-- It is also very compact fitting in a single 'Int'.
newtype Delta = Delta Int
  deriving (Eq, Ord, Data, Generic, Num)

instance Show Delta where
  showsPrec d (Delta n) = showsPrec d n

instance Read Delta where
  readPrec = Delta <$> readPrec

instance Hashable Delta

instance Default Delta where
  def = Delta def

instance Measured Delta where
  type Measure Delta = Delta
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
  delta :: t -> Delta

-- | extract the number of utf-16 code units from a delta
units :: HasDelta t => t -> Int
units y = case delta y of
  Delta x -> x

instance HasDelta Delta where
  delta = id

instance (Measured a, HasDelta (Measure a)) => HasDelta (FingerTree a) where
  delta = delta . measure

--------------------------------------------------------------------------------
-- Monoidal deltas
--------------------------------------------------------------------------------

-- |
-- 'delta' for this type is a monoid homomorphism
--
-- @
-- 'delta' (m '<>' n) = 'delta' m <> 'delta' n
-- 'delta' mempty = 0
-- @
class (Monoid t, HasDelta t) => HasMonoidalDelta t where
instance HasMonoidalDelta Delta
instance (Measured a, HasMonoidalDelta (Measure a)) => HasMonoidalDelta (FingerTree a)

--------------------------------------------------------------------------------
-- Monotone deltas
--------------------------------------------------------------------------------

-- |
-- Requires that 'delta' is monotone
--
-- @m <= n@ implies @'delta' m <= 'delta' n@
class (Ord t, HasDelta t) => HasOrderedDelta t
instance HasOrderedDelta Delta
