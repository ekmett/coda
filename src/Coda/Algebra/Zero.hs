{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

module Coda.Algebra.Zero
  ( SemigroupWithZero(..)
  , WithZero(..)
  ) where

import Data.Data hiding (Prefix)
import Data.Semigroup
import Data.String
import GHC.Generics hiding (Prefix)
import Prelude

-- | @
-- zero <> a = zero = a <> zero
-- @
class Semigroup a => SemigroupWithZero a where
  zero :: a

instance SemigroupWithZero All where
  zero = All False

instance SemigroupWithZero Any where
  zero = Any True

instance SemigroupWithZero a => SemigroupWithZero (Dual a) where
  zero = Dual zero

instance Num a => SemigroupWithZero (Product a) where
  zero = Product 0

instance (Ord a, Bounded a) => SemigroupWithZero (Max a) where
  zero = Max maxBound

instance (Ord a, Bounded a) => SemigroupWithZero (Min a) where
  zero = Min minBound

-- | pushout
class (SemigroupWithZero a, Monoid a) => MonoidWithZero a
instance (SemigroupWithZero a, Monoid a) => MonoidWithZero a

-- adjoin a zero element to a semigroup
data WithZero a = Zero | NonZero a
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic,Generic1,Functor,Foldable,Traversable)

instance Semigroup a => Semigroup (WithZero a) where
  Zero <> _ = Zero
  _ <> Zero = Zero
  NonZero a <> NonZero b = NonZero (a <> b)

instance Semigroup a => SemigroupWithZero (WithZero a) where
  zero = Zero

instance Monoid a => Monoid (WithZero a) where
  mempty = NonZero mempty
  mappend Zero _ = Zero
  mappend _ Zero = Zero
  mappend (NonZero a) (NonZero b) = NonZero (mappend a b)

instance IsString a => IsString (WithZero a) where
  fromString = NonZero . fromString
