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
  , Relative1(..)
  , Relative2(..)
  , frel1
  , frel2
  , GRelative(..)
  , GRelative1(..)
  ) where

import Coda.Relative.Delta
import Data.Bifunctor
import Data.Coerce
import Data.List.NonEmpty
import Data.Primitive.Array
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
-- 'rel' 'mempty' ≡ 'id'
-- 'rel' (m '<>' n) ≡ 'rel' m . 'rel' n
-- @
class Relative a where
  rel :: Delta -> a -> a
  default rel :: (Generic a, GRelative (Rep a)) => Delta -> a -> a
  rel d = to . grel d . from

instance Relative Delta where
  rel = (<>)

instance Relative a => Relative (Maybe a) where
  rel d (Just a) = Just (rel d a)
  rel _ Nothing = Nothing
  {-# inline rel #-}

instance Relative a => Relative [a]
instance Relative a => Relative (NonEmpty a)
instance Relative a => Relative (Array a) where
  rel = frel1 rel
  
class GRelative f where
  grel :: Delta -> f a -> f a

instance GRelative U1 where
  grel _ U1 = U1

instance GRelative V1 where
  grel _ x = case x of {}

instance (GRelative f, GRelative g) => GRelative (f :*: g) where
  grel d (f :*: g) = grel d f :*: grel d g
  
instance (GRelative f, GRelative g) => GRelative (f :+: g) where
  grel d (L1 f) = L1 (grel d f)
  grel d (R1 f) = R1 (grel d f)

instance Relative c => GRelative (K1 i c) where
  grel = coerce (rel @c)

instance GRelative f => GRelative (M1 i c f) where
  grel d = M1 #. grel d .# unM1

instance (Relative1 f, GRelative g) => GRelative (f :.: g) where
  grel d = Comp1 #. rel1 grel d .# unComp1

class Relative1 f where
  rel1 :: (Delta -> a -> a) -> Delta -> f a -> f a
  default rel1 :: (Generic1 f, GRelative1 (Rep1 f)) => (Delta -> a -> a) -> Delta -> f a -> f a
  rel1 k d = to1 . grel1 k d . from1

frel1 :: Functor f => (Delta -> a -> a) -> Delta -> f a -> f a
frel1 = fmap fmap

instance Relative1 []
-- instance Relative1 Maybe
-- instance Relative a => Relative1 ((,) a)
-- instance Relative a => Relative1 (Either a)
  
class GRelative1 f where
  grel1 :: (Delta -> a -> a) -> Delta -> f a -> f a

instance GRelative1 U1 where
  grel1 _ _ U1 = U1

instance GRelative1 V1 where
  grel1 _ _ x = case x of {}

instance (GRelative1 f, GRelative1 g) => GRelative1 (f :*: g) where
  grel1 k d (f :*: g) = grel1 k d f :*: grel1 k d g
  
instance (GRelative1 f, GRelative1 g) => GRelative1 (f :+: g) where
  grel1 k d (L1 f) = L1 (grel1 k d f)
  grel1 k d (R1 g) = R1 (grel1 k d g)

instance Relative c => GRelative1 (K1 i c) where
  grel1 _ = coerce (rel @c)

instance GRelative1 f => GRelative1 (M1 i c f) where
  grel1 k d = M1 #. grel1 k d .# unM1

instance GRelative1 Par1 where
  grel1 = coerce 

instance Relative1 f => GRelative1 (Rec1 f) where
  grel1 k d = Rec1 #. rel1 k d .# unRec1

--------------------------------------------------------------------------------
-- Relative2
--------------------------------------------------------------------------------

class Relative2 f where
  rel2 :: (Delta -> a -> a) -> (Delta -> b -> b) -> Delta -> f a b -> f a b
  default rel2 :: Bifunctor f => (Delta -> a -> a) -> (Delta -> b -> b) -> Delta -> f a b -> f a b
  rel2 f g d = bimap (f d) (g d) 

frel2 :: Bifunctor f => (Delta -> a -> a) -> (Delta -> b -> b) -> Delta -> f a b -> f a b
frel2 f g d = bimap (f d) (g d)

instance Relative2 (,)
instance Relative2 Either
