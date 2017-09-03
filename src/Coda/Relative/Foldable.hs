{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Coda.Relative.Foldable
  ( RelativeFoldable(..)
  , RelativeFoldableWithIndex(..)
  ) where

import Coda.Relative.Delta
import Coda.Relative.Class
import Control.Lens
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import qualified Data.Monoid as Monoid
import Data.Monoid hiding (Product(..), Sum(..))
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)

class RelativeFoldable f where
  rfoldMap :: Monoid m => (Delta -> a -> m) -> Delta -> f a -> m
  default rfoldMap :: (Foldable f, Monoid m) => (Delta -> a -> m) -> Delta -> f a -> m
  rfoldMap f !d = foldMap (f d)

  rfoldr :: (Delta -> a -> r -> r) -> r -> Delta -> f a -> r
  rfoldr f z !d = flip appEndo z . rfoldMap (\d' -> Endo #. f d') d

  rlength :: f a -> Int
  rlength = Monoid.getSum #. rfoldMap (\_ _ -> Monoid.Sum 1) 0

  rnull :: f a -> Bool
  rnull = getAny #. rfoldMap (\_ _ -> Any True) 0

  rtoList :: Relative a => f a -> [a]
  rtoList = rfoldr (\d a r -> rel d a : r) [] 0

instance RelativeFoldable Proxy
instance RelativeFoldable []
instance RelativeFoldable NonEmpty
instance RelativeFoldable (Map k)
instance RelativeFoldable Identity
instance RelativeFoldable ((,) a)
instance RelativeFoldable (Either a)

instance (RelativeFoldable f, RelativeFoldable g) => RelativeFoldable (Compose f g) where
  rfoldMap f !d = rfoldMap (rfoldMap f) d .# getCompose

instance (RelativeFoldable f, RelativeFoldable g) => RelativeFoldable (Product f g) where
  rfoldMap f !d (Pair x y) = rfoldMap f d x `mappend` rfoldMap f d y

instance (RelativeFoldable f, RelativeFoldable g) => RelativeFoldable (Sum f g) where
  rfoldMap f !d (InL x) = rfoldMap f d x
  rfoldMap f !d (InR y) = rfoldMap f d y

class RelativeFoldable f => RelativeFoldableWithIndex i f | f -> i where
  irfoldMap :: Monoid m => (Delta -> i -> a -> m) -> Delta -> f a -> m
  default irfoldMap :: (FoldableWithIndex i f, Monoid m) => (Delta -> i -> a -> m) -> Delta -> f a -> m
  irfoldMap f !d = ifoldMap (f d)

  irfoldr :: (Delta -> i -> a -> r -> r) -> r -> Delta -> f a -> r
  irfoldr f z !d = flip appEndo z . irfoldMap (\d' i -> Endo #. f d' i) d

instance RelativeFoldableWithIndex a ((,) a)
instance RelativeFoldableWithIndex Int []
instance RelativeFoldableWithIndex Int NonEmpty
instance RelativeFoldableWithIndex k (Map k) -- relocate keys?

instance (RelativeFoldableWithIndex i f, RelativeFoldableWithIndex j g) => RelativeFoldableWithIndex (Either i j) (Product f g) where
  irfoldMap f d (Pair x y)
      = irfoldMap (\d' -> f d' . Left) d x
     <> irfoldMap (\d' -> f d' . Right) d y

instance (RelativeFoldableWithIndex i f, RelativeFoldableWithIndex j g) => RelativeFoldableWithIndex (Either i j) (Sum f g) where
  irfoldMap f d (InL x) = irfoldMap (\d' -> f d' . Left) d x
  irfoldMap f d (InR y) = irfoldMap (\d' -> f d' . Right) d y

