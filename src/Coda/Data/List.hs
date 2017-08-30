{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
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

module Coda.Data.List
  ( List(..)
  , drop
  , flatten
  ) where

import Coda.Data.Queue as Queue
import Coda.Data.View
import Data.Foldable as Foldable
import Data.Function (on)
import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Lens hiding ((:<))
import Data.Semigroup
import GHC.Exts as Exts (IsList(..))
import Prelude hiding (drop)

-- | An Okasaki-style catenable list / output-restricted deque
--
-- 'mappend', 'cons', 'snoc', 'uncons' are all \(\mathcal{O}(1)\)
--
-- Use @('Empty', ':<')@ from @Control.Lens@ to get a traditional list like view.
data List a
  = Nil
  | Cons !a !(Queue (List a))
  deriving (Show, Functor, Traversable)

instance Eq a => Eq (List a) where
  (==) = (==) `on` Foldable.toList

instance Ord a => Ord (List a) where
  compare = compare `on` Foldable.toList

instance Foldable List where
  null Nil = True
  null _ = False

  foldMap _ Nil = mempty
  foldMap f (Cons a q) = f a `mappend` foldMap (foldMap f) q

instance Applicative List where
  pure a = Cons a Q0
  fs <*> as = foldMap (<$> as) fs

instance Monad List where
  m >>= f = foldMap f m

instance Alternative List where
  empty = Nil

  Nil      <|> xs  = xs
  xs       <|> Nil = xs
  Cons x q <|> ys  = Cons x (Queue.snoc q ys)

instance MonadPlus List where
  mzero = Nil
  mplus = (<|>)

instance Semigroup (List a) where
  (<>) = (<|>)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<|>)

link :: List a -> List a -> List a
link (Cons x q) s = Cons x (Queue.snoc q s)
link Nil s = s
{-# INLINE link #-}

linkAll :: List a -> Queue (List a) -> List a
linkAll t q' = case viewL q' of
  EmptyL -> t
  x :< q'' -> link t (linkAll x q'')

-- | Concatenate a queue of catenable lists
--
-- \(\mathcal{O}(1 + e)\) where @e@ is the number of empty entries in the queue
flatten :: Queue (List a) -> List a
flatten q = case viewL q of
  EmptyL -> Nil
  t :< q' -> linkAll t q'

instance ViewableL List where
  viewL Nil        = EmptyL
  viewL (Cons a q) = a :< flatten q

instance Cons (List a) (List b) a b where
  _Cons = prism embed project where
    embed :: (a, List a) -> List a
    embed (a, as) = Cons a Q0 <> as
    project :: List a -> Either (List b) (a, List a)
    project Nil = Left Nil
    project (Cons a q) = Right (a, flatten q)

instance AsEmpty (List a) where
  _Empty = prism (const Nil) $ \case
    Nil -> Right ()
    x   -> Left x

instance MonadZip List where
  mzipWith f m n = case viewL m of
    a :< m' -> case viewL n of
      b :< n' -> f a b `cons` mzipWith f m' n'
      EmptyL -> Nil
    EmptyL -> Nil

instance IsList (List a) where
  type Item (List a) = a
  fromList = foldr cons Nil
  toList = Foldable.toList

-- | \(\mathcal{O}(k)\) in @k@ the number of characters being dropped.
drop :: Int -> List a -> List a
drop 0 !xs = xs
drop _ Nil  = Nil
drop n (Cons _ q) = drop (n-1) (flatten q)
