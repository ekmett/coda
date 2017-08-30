{-# language EmptyCase #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language DefaultSignatures #-}
{-# language ScopedTypeVariables #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Relative.Class
  ( Relative(..)
  , GRelative
  , grel
  , frel
  , birel
  ) where

import Coda.Relative.Delta
import Data.Bifunctor
import Data.Coerce
import Data.List.NonEmpty
import Data.Profunctor.Unsafe
import Data.Semigroup
import GHC.Generics

--------------------------------------------------------------------------------
-- Relative
--------------------------------------------------------------------------------

-- | Applying a relative position change as a left monoid action
--
-- Laws:
--
-- @
-- 'rel' 0 ≡ 'id'
-- 'rel' (m '+' n) ≡ 'rel' m . 'rel' n
-- @
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

instance (Relative a, Relative b) => Relative (a, b) where rel = birel
instance (Relative a, Relative b) => Relative (Either a b) where rel = birel
instance Relative a => Relative [a] where rel = frel
instance Relative a => Relative (NonEmpty a) where rel = frel
  
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

grel :: (Generic a, GRelative (Rep a)) => Delta -> a -> a
grel d = to . grel' d . from
{-# INLINE grel #-}

frel :: (Functor f, Relative a) => Delta -> f a -> f a
frel = fmap fmap rel
{-# INLINE frel #-}

birel :: (Bifunctor f, Relative a, Relative b) => Delta -> f a b -> f a b
birel d = bimap (rel d) (rel d)

