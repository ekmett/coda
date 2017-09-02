{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Coda.Relative.Foldable 
  ( RelativeFoldable(..)
  , RelativeFoldableWithIndex(..)
  ) where

import Coda.Relative.Delta
import Coda.Relative.Class
import Control.Lens
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)

class RelativeFoldable f where
  -- |
  -- @
  -- 'rfoldMap' (d '<>' d') f = 'rfoldMap' d (f '.' 'rel' d')
  -- @
  rfoldMap :: (Relative a, Monoid m) => Delta -> (a -> m) -> f a -> m
  default rfoldMap :: (Foldable f, Relative a, Monoid m) => Delta -> (a -> m) -> f a -> m
  rfoldMap !d f = foldMap (f . rel d)

  -- |
  -- @
  -- 'rfoldr' (d '<>' d') f = 'rfoldr' d (f . 'rel' d')
  -- @
  rfoldr :: Relative a => Delta -> (a -> r -> r) -> r -> f a -> r
  rfoldr !d f z = flip appEndo z . rfoldMap d (Endo #. f)

  rlength :: f a -> Int
  default rlength :: Foldable f => f a -> Int
  rlength = length

  rnull :: f a -> Bool
  default rnull :: Foldable f => f a -> Bool
  rnull = null

  rtoList :: Relative a => f a -> [a]
  rtoList = rfoldr mempty (:) []

instance RelativeFoldable Proxy
instance RelativeFoldable []
instance RelativeFoldable NonEmpty
instance RelativeFoldable (Map k)
instance RelativeFoldable Identity
instance RelativeFoldable ((,) a)
instance RelativeFoldable (Either a)

class RelativeFoldable f => RelativeFoldableWithIndex i f | f -> i where
  -- |
  -- @
  -- 'irfoldMap' (d '<>' d') f = 'irfoldMap' d (\i -> f i . 'rel' d')
  -- @
  irfoldMap :: (Relative a, Monoid m) => Delta -> (i -> a -> m) -> f a -> m
  default irfoldMap :: (FoldableWithIndex i f, Relative a, Monoid m) => Delta -> (i -> a -> m) -> f a -> m
  irfoldMap !d f = ifoldMap (\i -> f i . rel d)

  -- |
  -- @
  -- 'irfoldr' (d '<>' d') f = 'irfoldr' d (\i -> f i . 'rel' d')
  -- @
  irfoldr :: Relative a => Delta -> (i -> a -> r -> r) -> r -> f a -> r
  irfoldr !d f z = flip appEndo z . irfoldMap d (\i -> Endo #. f i)

instance RelativeFoldableWithIndex a ((,) a)
instance RelativeFoldableWithIndex Int []
instance RelativeFoldableWithIndex Int NonEmpty
instance RelativeFoldableWithIndex k (Map k) -- relocate keys?
