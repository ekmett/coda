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

module Coda.Data.Seq
  ( Seq(..)
  , drop
  , flatten
  ) where

import Coda.Data.Queue as Queue
import Coda.Data.View
import Data.Default
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
data Seq a
  = Nil
  | Cons !a !(Queue (Seq a))
  deriving (Show, Functor, Traversable)

instance Default (Seq a) where
  def = Nil

instance Eq a => Eq (Seq a) where
  (==) = (==) `on` Foldable.toList

instance Ord a => Ord (Seq a) where
  compare = compare `on` Foldable.toList

instance Foldable Seq where
  null Nil = True
  null _ = False

  foldMap _ Nil = mempty
  foldMap f (Cons a q) = f a `mappend` foldMap (foldMap f) q

instance Applicative Seq where
  pure a = Cons a Q0
  fs <*> as = foldMap (<$> as) fs

instance Monad Seq where
  m >>= f = foldMap f m

instance Alternative Seq where
  empty = Nil

  Nil      <|> xs  = xs
  xs       <|> Nil = xs
  Cons x q <|> ys  = Cons x (Queue.snoc q ys)

instance MonadPlus Seq where
  mzero = Nil
  mplus = (<|>)

instance Semigroup (Seq a) where
  (<>) = (<|>)

instance Monoid (Seq a) where
  mempty = Nil
  mappend = (<|>)

link :: Seq a -> Seq a -> Seq a
link (Cons x q) s = Cons x (Queue.snoc q s)
link Nil s = s
{-# INLINE link #-}

linkAll :: Seq a -> Queue (Seq a) -> Seq a
linkAll t q' = case viewL q' of
  EmptyL -> t
  x :< q'' -> link t (linkAll x q'')

-- | Concatenate a queue of catenable lists
--
-- \(\mathcal{O}(1 + e)\) where @e@ is the number of empty entries in the queue
flatten :: Queue (Seq a) -> Seq a
flatten q = case viewL q of
  EmptyL -> Nil
  t :< q' -> linkAll t q'

instance ViewableL Seq where
  viewL Nil        = EmptyL
  viewL (Cons a q) = a :< flatten q

instance Cons (Seq a) (Seq b) a b where
  _Cons = prism embed project where
    embed :: (a, Seq a) -> Seq a
    embed (a, as) = Cons a Q0 <> as
    project :: Seq a -> Either (Seq b) (a, Seq a)
    project Nil = Left Nil
    project (Cons a q) = Right (a, flatten q)

instance AsEmpty (Seq a) where
  _Empty = prism (const Nil) $ \case
    Nil -> Right ()
    x   -> Left x

instance MonadZip Seq where
  mzipWith f m n = case viewL m of
    a :< m' -> case viewL n of
      b :< n' -> f a b `cons` mzipWith f m' n'
      EmptyL -> Nil
    EmptyL -> Nil

instance IsList (Seq a) where
  type Item (Seq a) = a
  fromList = foldr cons Nil
  toList = Foldable.toList

-- | \(\mathcal{O}(k)\) in @k@ the number of characters being dropped.
drop :: Int -> Seq a -> Seq a
drop 0 !xs = xs
drop _ Nil  = Nil
drop n (Cons _ q) = drop (n-1) (flatten q)
