{-# language DefaultSignatures #-}

module Coda.Relative.Functor 
  ( RelativeFunctor(..)
  , RelativeBifunctor(..)
  ) where

import Data.Bifunctor
import Coda.Relative.Class
-- import Data.Map.Strict
-- import Data.HashMap.Strict

-- |
-- @
-- We define a relative morphism to be a function
-- @f@ that satisfies 
--
-- @
-- f . rel d = rel d . f
-- @

-- A relative functor is an endofunctor on the category of
-- relative morphisms.
--
-- That is to say, given such an @f@
--
-- @
-- relmap f . rel d = rel d . relmap f
-- relmap id = id
-- relmap f . relmap g = relmap (f . g)
-- @
--
-- Every functor can be made a relative functor
-- by defining @rel = fmap . rel@, then the
-- above conditions follow from the free theorem
-- for @fmap@.
--
-- These should still be used sparingly as they arent likely to be efficient
class RelativeFunctor f where
  relmap :: (a -> b) -> f a -> f b
  default relmap :: Functor f => (a -> b) -> f a -> f b
  relmap = fmap

instance RelativeFunctor []
instance RelativeFunctor Maybe
instance Relative a => RelativeFunctor ((,)a) -- unused but necessary constraint
instance Relative a => RelativeFunctor (Either a) -- unused but necessary constraint

-- inefficient
--instance Ord k => RelativeFunctor (Map k)
--instance RelativeFunctor IntMap
--instance Hashable k => RelativeFunctor (HashMap k)

-- | A bifunctor on the category of relative morphisms.
class RelativeBifunctor p where
  birelmap :: (a -> b) -> (c -> d) -> p a c -> p b d
  default birelmap :: Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d
  birelmap = bimap

instance RelativeBifunctor (,)
instance RelativeBifunctor Either
