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

module Coda.Syntax.Multi
  ( Multi(..)
  , drop
  -- * Internals
  , remainder
  , Q, snocQ
  ) where

import Coda.Syntax.View
import Data.Foldable as Foldable
import Data.Function (on)
import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Lens hiding ((:<), view)
import Data.Semigroup
import GHC.Exts as Exts (IsList(..))
import Prelude hiding (drop)

data P a = P a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data B a
  = B1 a
  | B2 !(P a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Q a
  = Q0
  | Q1 a
  | QN !(B a) !(Q (P a)) !(B a)
  deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Q a) where
  (==) = (==) `on` Foldable.toList

instance Ord a => Ord (Q a) where
  compare = compare `on` Foldable.toList

snocQ :: Q a -> a -> Q a
snocQ Q0 b = Q1 b
snocQ (Q1 a) b = QN (B1 a) Q0 (B1 b)
snocQ (QN l m (B1 a)) b = QN l m (B2 (P a b))
snocQ (QN l m (B2 r)) b = QN l (snocQ m r) (B1 b)

instance ViewableL Q where
  viewL Q0 = EmptyL
  viewL (Q1 a) = a :< Q0
  viewL (QN (B2 (P a b)) m r) = a :< QN (B1 b) m r
  viewL (QN (B1 a) m r) = a :< case viewL m of
    l :< m' -> QN (B2 l) m' r
    EmptyL -> case r of
      B1 x -> Q1 x
      B2 (P x y) -> QN (B1 x) Q0 (B1 y)

-- | An Okasaki-style catenable list
--
-- 'mappend', 'cons', 'snoc', 'uncons' are all \(\mathcal{O}(1)\)
data Multi a
  = Nil
  | Cons !a !(Q (Multi a))
  deriving (Show, Functor, Traversable)

instance Eq a => Eq (Multi a) where
  (==) = (==) `on` Foldable.toList

instance Ord a => Ord (Multi a) where
  compare = compare `on` Foldable.toList

instance Foldable Multi where
  null Nil = True
  null _ = False
  foldMap _ Nil = mempty
  foldMap f (Cons a q) = f a `mappend` foldMap (foldMap f) q

instance Applicative Multi where
  pure a = Cons a Q0
  fs <*> as = foldMap (<$> as) fs

instance Monad Multi where
  m >>= f = foldMap f m

instance Alternative Multi where
  empty = Nil
  Nil <|> xs = xs
  xs <|> Nil = xs
  xs <|> ys  = link xs ys

instance MonadPlus Multi where
  mzero = Nil
  mplus = (<|>)

instance Semigroup (Multi a) where
  (<>) = (<|>)

instance Monoid (Multi a) where
  mempty = Nil
  mappend = (<|>)

link :: Multi a -> Multi a -> Multi a
link (Cons x q) s = Cons x (snocQ q s)
link Nil s = s -- never happens

linkAll :: Multi a -> Q (Multi a) -> Multi a
linkAll t q' = case viewL q' of
  EmptyL -> t
  x :< q'' -> link t (linkAll x q'')

-- |
-- \(\mathcal{O}(1 + e)\) where @e@ is the number of empty entries in Q
remainder :: Q (Multi a) -> Multi a
remainder q = case viewL q of
  EmptyL -> Nil
  t :< q' -> linkAll t q'

instance ViewableL Multi where
  viewL Nil        = EmptyL
  viewL (Cons a q) = a :< remainder q

instance Cons (Multi a) (Multi b) a b where
  _Cons = prism embed project where
    embed :: (a, Multi a) -> Multi a
    embed (a, as) = Cons a Q0 <> as
    project :: Multi a -> Either (Multi b) (a, Multi a)
    project Nil = Left Nil
    project (Cons a q) = Right (a, remainder q)

instance AsEmpty (Multi a) where
  _Empty = prism (const Nil) $ \case
    Nil -> Right ()
    x   -> Left x

instance MonadZip Multi where
  mzipWith f m n = case viewL m of
    a :< m' -> case viewL n of
      b :< n' -> f a b `cons` mzipWith f m' n'
      _ -> Nil
    _ -> Nil

instance IsList (Multi a) where
  type Item (Multi a) = a
  fromList = foldr cons Nil
  toList = Foldable.toList

-- | \(\mathcal{O}(k)\) in @k@ the number of characters being dropped.
drop :: Int -> Multi a -> Multi a
drop 0 !xs = xs
drop _ Nil  = Nil
drop n (Cons _ q) = drop (n-1) (remainder q)
