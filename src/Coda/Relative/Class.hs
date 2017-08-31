{-# language EmptyCase #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language DefaultSignatures #-}
{-# language ScopedTypeVariables #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- In order to work with fragments of syntax trees without having to pay
-- an inordinate cost moving around errors and terms, we need a notion of
-- 'Relative' data types that permit relocation cheaply.
--
-- Note: There is structure to these classes: Consider the following
-- problematic derivation that show some instances here are incompatible
-- with others.
--
-- @
-- 'Delta' 1 =                             -- by instance 'Monoid' 'Delta'
-- 'Delta' 1 '<>' 'mempty' =               -- by instance 'MonoidalDelta' t
-- 'Delta' 1 '<>' 'delta' 'mempty' =       -- by instance 'RelativeDelta' t
-- 'delta' ('rel' ('Delta' 1) 'mempty') =  -- by instance 'RelativeMonoid' t
-- 'delta' 'mempty' =                      -- by instance 'MonoidalDelta' t
-- 'mempty' =                              -- by definition
-- 'Delta' 0
-- @
---------------------------------------------------------------------------------

module Coda.Relative.Class
  (
  -- * Data types with relative positions
    Relative(..)
  , GRelative
  , grel
  , frel
  , birel

  -- * Relative monoids
  , RelativeMonoid

  -- * Relative orderings
  , RelativeOrder
  , StrictRelativeOrder

  -- * ordered monoids
  , OrderedMonoid

  -- * Relative deltas
  , HasRelativeDelta
  ) where


import Coda.Relative.Delta
import Data.Bifunctor
import Data.Coerce
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Void
import GHC.Generics

--------------------------------------------------------------------------------
-- Relative
--------------------------------------------------------------------------------

-- | Applying a relative position change as a left monoid action
--
-- Laws:
--
-- @
-- 'rel' mempty ≡ 'id'
-- 'rel' (m '<>' n) ≡ 'rel' m . 'rel' n
-- @
--
-- Preferably 'rel' relocates in /O(1)/ or logarithmic time at worst or
-- the container probably isn't well suited for relative use.
class Relative a where
  rel :: Delta -> a -> a
  default rel :: (Generic a, GRelative (Rep a)) => Delta -> a -> a
  rel = grel

instance Relative Delta where
  rel = (<>)

instance Relative a => Relative (Maybe a) where
  rel d (Just a) = Just (rel d a)
  rel _ Nothing = Nothing
  {-# inline rel #-}

instance Relative ()
instance Relative Void
instance (Relative a, Relative b) => Relative (a, b) where rel = birel
instance (Relative a, Relative b) => Relative (Either a b) where rel = birel

-- instance Relative a => Relative [a] where rel = frel
-- instance Relative a => Relative (NonEmpty a) where rel = frel

class GRelative f where
  grel' :: Delta -> f a -> f a

instance GRelative U1 where
  grel' _ U1 = U1

instance GRelative V1 where
  grel' _ x = case x of {}

instance (GRelative f, GRelative g) => GRelative (f :*: g) where
  grel' d (f :*: g) = grel' d f :*: grel' d g

instance (GRelative f, GRelative g) => GRelative (f :+: g) where
  grel' d (L1 f) = L1 (grel' d f)
  grel' d (R1 f) = R1 (grel' d f)

instance Relative c => GRelative (K1 i c) where
  grel' = coerce (rel @c)

instance GRelative f => GRelative (M1 i c f) where
  grel' d = M1 #. grel' d .# unM1

instance (Functor f, GRelative g) => GRelative (f :.: g) where
  grel' d = Comp1 #. fmap fmap grel' d .# unComp1

-- | We can derive relativity generically.
grel :: (Generic a, GRelative (Rep a)) => Delta -> a -> a
grel d = to . grel' d . from
{-# INLINE grel #-}

-- | Every functor can be relative.
frel :: (Functor f, Relative a) => Delta -> f a -> f a
frel = fmap fmap rel
{-# INLINE frel #-}

-- | Every bifunctor can be relative.
birel :: (Bifunctor f, Relative a, Relative b) => Delta -> f a b -> f a b
birel d = bimap (rel d) (rel d)

--------------------------------------------------------------------------------
-- Relative monoids
--------------------------------------------------------------------------------

-- |
-- Laws: @'rel' d@ is a monoid homomorphism.
--
-- @
-- 'rel' d (m '<>' n)   = rel d m '<>' rel d n
-- 'rel' d 'mempty'   = 'mempty'
-- @
--
-- TODO: Add @RelativeSemigroup@ in ghc 8.4
--
-- @
-- instance RelativeSemigroup Void
-- @
class (Relative t, Monoid t) => RelativeMonoid t

instance RelativeMonoid Delta
instance RelativeMonoid ()
instance (RelativeMonoid a, RelativeMonoid b) => RelativeMonoid (a,b)

--------------------------------------------------------------------------------
-- Relative orderings
--------------------------------------------------------------------------------

-- | A relative order
--
-- Laws:
--
-- @'rel' d@ is monotone, that is to say
--
-- @x '<=' y@ implies @'rel' d x <= 'rel' d y@
--
--
class (Relative t, Ord t) => RelativeOrder t

instance RelativeOrder Delta
instance RelativeOrder ()
instance (RelativeOrder a, RelativeOrder b) => RelativeOrder (a, b)
instance (RelativeOrder a, RelativeOrder b) => RelativeOrder (Either a b)


-- | A _strict_ relative order
--
--
-- Laws:
--
-- @x '<' y@ implies @'rel' d x '<' 'rel' d y@
--
-- This can be useful for keys in relative maps because shifting the
-- map can't cause keys to collapse
class RelativeOrder t => StrictRelativeOrder t

instance StrictRelativeOrder Delta
instance StrictRelativeOrder ()
instance (StrictRelativeOrder a, StrictRelativeOrder b) => StrictRelativeOrder (a, b)
instance (StrictRelativeOrder a, StrictRelativeOrder b) => StrictRelativeOrder (Either a b)

--------------------------------------------------------------------------------
-- Ordered monoids
--------------------------------------------------------------------------------

-- |
-- An <https://en.wikipedia.org/wiki/Ordered_semigroup ordered monoid>.
--
-- @x '<=' y@ implies @z '<>' x '<=' z '<>' y@ and @x '<>' z '<=' y' <>' z@
class (Ord t, Monoid t) => OrderedMonoid t where

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

-- TODO: ghc 8.2
-- instance (Ord a, Bounded a) => OrderedMonoid (Min a)
-- instance (Ord a, Bounded a) => OrderedMonoid (Max a)

-- TODO: ghc 8.4
-- instance Ord a => OrderedSemigroup (NonEmpty a)

--------------------------------------------------------------------------------
-- Relative deltas
--------------------------------------------------------------------------------

-- |
-- 'delta' and 'rel'
--
-- @
-- 'delta' ('rel' d p) = d <> 'delta' p
-- @
class (Relative t, HasDelta t) => HasRelativeDelta t where
instance HasRelativeDelta Delta
