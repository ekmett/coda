{-# language CPP #-}
{-# language EmptyCase #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language DefaultSignatures #-}
{-# language ScopedTypeVariables #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD-2-Clause OR Apache-2.0
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

module Relative
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
  ) where

import Data.Bifunctor
import Data.Coerce
import Data.Functor.Identity
import Data.List.NonEmpty
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Void
import GHC.Generics

import Delta

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
--
-- Note: rel d = id is a perfectly legitimate definition by these laws.
--
-- Note: if you use some stashed delta to slow the descent into your data
-- structure, then you probably need to have nominal roles for your arguments.
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
instance Relative (Proxy a)
instance Relative a => Relative (Identity a)
instance (Relative a, Relative b) => Relative (a, b) where rel = birel
instance (Relative a, Relative b) => Relative (Either a b) where rel = birel
-- | /O(n)/
instance Relative a => Relative [a]
-- | /O(n)/
instance Relative a => Relative (NonEmpty a)

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
grel 0 = id
grel d = to . grel' d . from
{-# INLINE grel #-}

-- | Every functor can be relative.
frel :: (Functor f, Relative a) => Delta -> f a -> f a
frel 0 = id
frel f = fmap (rel f)
{-# INLINE frel #-}

-- | Every bifunctor can be relative.
birel :: (Bifunctor f, Relative a, Relative b) => Delta -> f a b -> f a b
birel 0 = id
birel d = bimap (rel d) (rel d)
{-# inline birel #-}

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
-- instance Relative a => RelativeSemigroup (NonEmpty a)
-- @
class (Relative t, Monoid t) => RelativeMonoid t

instance RelativeMonoid Delta
instance RelativeMonoid ()
instance (RelativeMonoid a, RelativeMonoid b) => RelativeMonoid (a,b)
instance Relative a => RelativeMonoid [a]

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
instance RelativeOrder a => RelativeOrder [a]
instance RelativeOrder a => RelativeOrder (NonEmpty a)

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
instance StrictRelativeOrder a => StrictRelativeOrder [a]
instance StrictRelativeOrder a => StrictRelativeOrder (NonEmpty a)
