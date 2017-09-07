{-# language LambdaCase #-}
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

module Coda.Data.Queue
  ( Queue(..)
  , snoc
  , uncons
  ) where

import Coda.Data.View
import Data.Default
import Data.Foldable as Foldable
import Data.Function (on)
import Control.Lens (prism, AsEmpty(..))
import GHC.Exts as Exts (IsList(..))

data P a = P a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data B a
  = B1 a
  | B2 !(P a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A fast functional queue
data Queue a
  = Q0
  | Q1 a
  | QN !(B a) !(Queue (P a)) !(B a)
  deriving (Show, Functor, Traversable)

instance Default (Queue a) where
  def = Q0

instance Foldable Queue where
  null Q0 = True
  null _ = False
  foldMap _ Q0 = mempty
  foldMap f (Q1 a) = f a
  foldMap f (QN l m r) = foldMap f l `mappend` foldMap (foldMap f) m `mappend` foldMap f r

instance AsEmpty (Queue a) where
  _Empty = prism (\() -> Q0) $ \case
    Q0 -> Right ()
    q -> Left q

instance IsList (Queue a) where
  type Item (Queue a) = a
  fromList = foldl' snoc Q0
  toList = Foldable.toList

instance Eq a => Eq (Queue a) where
  (==) = (==) `on` Foldable.toList

instance Ord a => Ord (Queue a) where
  compare = compare `on` Foldable.toList

instance ViewableL Queue where
  viewL Q0 = EmptyL
  viewL (Q1 a) = a :< Q0
  viewL (QN (B2 (P a b)) m r) = a :< QN (B1 b) m r
  viewL (QN (B1 a) m r) = a :< case viewL m of
    l :< m' -> QN (B2 l) m' r
    EmptyL -> case r of
      B1 x -> Q1 x
      B2 (P x y) -> QN (B1 x) Q0 (B1 y)

-- | \(\mathcal{O}(1)\)
snoc :: Queue a -> a -> Queue a
snoc Q0 b = Q1 b
snoc (Q1 a) b = QN (B1 a) Q0 (B1 b)
snoc (QN l m (B1 a)) b = QN l m (B2 (P a b))
snoc (QN l m (B2 r)) b = QN l (snoc m r) (B1 b)

-- | \(\mathcal{O}(1)\)
uncons :: Queue a -> Maybe (a, Queue a)
uncons q = case viewL q of
  EmptyL -> Nothing
  a :< r -> Just (a, r)
