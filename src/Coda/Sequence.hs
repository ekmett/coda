{-# language CPP #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Coda.Sequence 
  ( Seq(Empty,(:<),(:>))
  , drop, split, take
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens as Lens
import Data.Foldable as Foldable (toList)
import Data.Function (on)
import Data.Functor.Day.Curried
import Data.Functor.Yoneda
import Data.Semigroup
import Data.Traversable (foldMapDefault, fmapDefault)
import GHC.Exts (IsList(..))
import Text.Read
import qualified Data.FingerTree as F
import Data.FingerTree (FingerTree, Measured(..))
import Prelude hiding (drop, take)

--------------------------------------------------------------------------------
-- Weights and Measures
--------------------------------------------------------------------------------

data W = W !Int !Int deriving (Eq,Ord,Show,Read)

instance Num W where
  W a b + W c d = W (a + c) (b + d)
  W a b - W c d = W (a - c) (b - d)
  W a b * W c d = W (a * c) (b * d)
  negate (W a b) = W (negate a) (negate b)
  abs (W a b) = W (abs a) (abs b)
  signum (W a b) = W (signum a) (signum b)
  fromInteger i = W (fromInteger i) (fromInteger i)

count :: Measured W a => a -> Int
count a = case measure a of
  W c _ -> c

weight :: Measured W a => a -> Int
weight a = case measure a of
  W _ w -> w

instance Semigroup W where
  W a b <> W c d = W (a + c) (b + d)

instance Monoid W where
  mempty = W 0 0
  mappend = (<>)

instance Measured W W where measure = id

--------------------------------------------------------------------------------
-- Losing Count
--------------------------------------------------------------------------------

-- keeps weight, but resets count to 1
newtype U a = U a deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

makePrisms ''U

instance Measured W a => Measured W (U a) where
  measure (U a) = case measure a of
    W _ w -> W 1 w

--------------------------------------------------------------------------------
-- Bootstrapping FingerTrees
--------------------------------------------------------------------------------

type Q = FingerTree W

-- an O(1) catenable tree with O(log n) weighted indexing
--
-- Invariant: whenever you have @CD l m r@ both @l@ and @r@ are 'large'
data C a = CS !(Q a) | CD !(Q a) (C (U (Q a))) !(Q a)
  deriving Foldable

instance Measured W a => Measured W (C a) where
  measure (CS q)     = measure q
  measure (CD l m r) = measure l + measure m + measure r

large :: Measured W a => Q a -> Bool
large l = count l >= 2

-- | Restore the 'large' invariant on the left
deepL :: Measured W a => Q a -> C (U (Q a)) -> Q a -> C a
deepL l m r
  | large l   = CD l m r
  | otherwise = case uncons m of
    Nothing         -> CS (l F.>< r)
    Just (U l', m') -> CD (l F.>< l') m' r

-- | Restore the 'large' invariant on the right
deepR :: Measured W a => Q a -> C (U (Q a)) -> Q a -> C a
deepR l m r
  | large r   = CD l m r
  | otherwise = case unsnoc m of
    Nothing         -> CS (l F.>< r)
    Just (m', U r') -> CD l m' (r' F.>< r)

instance (Measured W a, Measured W b) => Cons (C a) (C b) a b where
  _Cons = prism kons unkons where
    kons (a, CS q)     = CS (a F.<| q)
    kons (a, CD l m r) = CD (a F.<| l) m r
    unkons (CS q) = case F.viewl q of
      F.EmptyL  -> Left (CS mempty)
      a F.:< q' -> Right (a, CS q')
    unkons (CD l m r) = case F.viewl l of
      a F.:< l' -> Right (a, deepL l' m r)
      F.EmptyL -> error "invariant violated"
      
instance (Measured W a, Measured W b) => Snoc (C a) (C b) a b where
  _Snoc = prism snok unsnok where
    snok (CS q, a)     = CS (q F.|> a)
    snok (CD l m r, a) = CD l m (r F.|> a)
    unsnok (CS q) = case F.viewr q of
      F.EmptyR  -> Left (CS mempty)
      q' F.:> a -> Right (CS q', a)
    unsnok (CD l m r) = case F.viewr r of
      F.EmptyR -> error "invariant violated"
      r' F.:> a -> Right (deepR l m r', a)

-- weighted split
wsplit :: Measured W a => Int -> C a -> (C a, C a)
wsplit w (CS q) = case F.split (\x -> weight x > w) q of
  (l, r) -> (CS l, CS r)
wsplit w (CD l m r)
  | w <= wl = case F.split (\x -> weight x > w) l of
    (ll, lr) -> (CS ll, deepL lr m r)
  | w < wlm = case wsplit (w-wl) m of
      (ml, mr) -> case unsnoc ml of
        Nothing -> (CS l, deepL mempty mr r)
        Just (mll, U mlr) -> case F.split (\x -> weight x > w - wl - weight mll) mlr of
          (mlrl,mlrr) -> (deepR l mll mlrl, deepL mlrr mr r)
  | otherwise = case F.split (\x -> weight x > w-wlm) r of
     (rl,rr) -> (deepR l m rl, CS rr)
  where wl = weight l
        wlm = wl + weight m

instance Measured W a => Semigroup (C a) where
  CS l <> CS r = CS (l F.>< r)
  CS q <> CD l m r = CD (q F.>< l) m r
  CD l m r <> CS q = CD l m (r F.>< q)
  CD l m r <> CD l' m' r' = CD l (snoc m (U r) <> cons (U l') m') r'

instance Measured W a => Monoid (C a) where
  mempty = CS mempty
  mappend = (<>)

instance Measured W a => IsList (C a) where
  type Item (C a) = a
  fromList = foldr cons mempty
  toList = Foldable.toList

instance Eq a => Eq (C a) where (==) = (==) `on` Foldable.toList
instance Ord a => Ord (C a) where compare = compare `on` Foldable.toList
instance Show a => Show (C a) where showsPrec d = showsPrec d . Foldable.toList
instance (Measured W a, Read a) => Read (C a) where readPrec = fromList <$> readPrec

--------------------------------------------------------------------------------
-- The loneliest number
--------------------------------------------------------------------------------

-- | Everything can be weighted with weight 1, count 1
newtype One a = One a deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

makePrisms ''One

instance Measured W (One a) where
  measure _ = W 1 1

--------------------------------------------------------------------------------
-- Sequences
--------------------------------------------------------------------------------

-- | O(1) Catenable deques with O(log n) time indexing
--
-- Most of the API is provided by standard combinators:
--
-- @
-- 'null', 'length', 'empty', 'pure', ('<|>') or ('<>'), etc.
-- @
--
-- Many other combinators and traversals come from @lens@:
--
-- @
-- 'cons', 'uncons', 'snoc', 'unsnoc', '_head', '_tail', 'ix', 'itraversed'
-- @
--
-- as well as the patterns for (':<'), (':>') and 'Empty'
--
-- The @OverloadedLists@ extension can be used as well.

newtype Seq a = Seq (C (One a)) deriving (Eq,Ord,Semigroup,Monoid,Measured W)

#if __GLASGOW_HASKELL__ >= 802
{-# complete_patterns ((:<), Empty) | (:>),Empty) #-}
#endif

makePrisms ''Seq

instance Show a => Show (Seq a) where
  showsPrec d = showsPrec d . Foldable.toList

instance Read a => Read (Seq a) where
  readPrec = fromList <$> readPrec

instance AsEmpty (Seq a) where
  _Empty = nearly empty null
  {-# inline _Empty #-}

instance Applicative Seq where
  pure a = Seq (CS (F.singleton (One a)))
  {-# inline pure #-}
  mf <*> ma = foldMap (\a -> ($ a) <$> mf) ma
  {-# inline (<*>) #-}
  (*>) = (>>) 
  {-# inline (*>) #-}

instance Monad Seq where
  (>>=) = flip foldMap 
  {-# inline (>>=) #-}

instance Alternative Seq where
  empty = Seq (CS F.empty)
  {-# inline empty #-}
  (<|>) = (<>)
  {-# inline (<|>) #-}
  
instance MonadPlus Seq where
  mzero = empty
  mplus = (<|>)

instance Cons (Seq a) (Seq b) a b where
  _Cons = _Seq._Cons.bimapping _One (from _Seq)
  {-# inline _Cons #-}

instance Snoc (Seq a) (Seq b) a b where
  _Snoc = _Seq._Snoc.bimapping (from _Seq) _One
  {-# inline _Snoc #-}

instance IsList (Seq a) where
  type Item (Seq a) = a
  fromList = foldr cons mempty
  toList = Foldable.toList

instance Functor Seq where
  fmap = fmapDefault

instance Foldable Seq where
  foldMap = foldMapDefault

  length = weight

  null (Seq (CS q)) = null q
  null _ = False

split :: Int -> Seq a -> (Seq a, Seq a)
split i (Seq q) = case wsplit i q of
  (l, r) -> (Seq l, Seq r)

take :: Int -> Seq a -> Seq a
take i q = fst (split i q)

drop :: Int -> Seq a -> Seq a
drop i q = snd (split i q)

instance Traversable Seq where
  traverse f0 xs = confusing (_Seq.traverseC._One) f0 xs where
    traverseC :: Applicative f => (a -> f b) -> C a -> f (C b)
    traverseC f (CS q)     = CS <$> F.unsafeTraverse f q
    traverseC f (CD l m r) = CD <$> F.unsafeTraverse f l <*> traverseC (_U (F.unsafeTraverse f)) m <*> F.unsafeTraverse f r
  {-# inline traverse #-}

instance FoldableWithIndex Int Seq
instance FunctorWithIndex Int Seq
instance TraversableWithIndex Int Seq where
  itraverse f0 (Seq xs) = lowerYoneda (lowerCurried (Seq <$> atC (atOne (\i -> liftCY . f0 i)) 0 xs)) where
    liftCY :: Applicative f => f a -> Curried (Yoneda f) (Yoneda f) a
    liftCY fa = Curried $ \(Yoneda k) -> Yoneda (\ab_r -> k (ab_r .) <*> fa)
    {-# inline liftCY #-}

    atOne :: Applicative f => (Int -> a -> f b) -> Int -> One a -> f (One b)
    atOne f !acc (One a) = One <$> f acc a
    {-# inline atOne #-}

    atU :: Applicative f => (Int -> a -> f b) -> Int -> U a -> f (U b)
    atU f !acc (U a) = U <$> f acc a
    {-# inline atU #-}

    atQ :: (Applicative f, Measured W a, Measured W b) => (Int -> a -> f b) -> Int -> Q a -> f (Q b)
    atQ f i q = F.traverseWithPos (\(W j _) a -> (f $! i + j) a) q
    {-# inline atQ #-}

    atC :: (Applicative f, Measured W a, Measured W b) => (Int -> a -> f b) -> Int -> C a -> f (C b)
    atC f acc (CS q) = CS <$> atQ f acc q
    atC f acc (CD l m r)
      | wl <- weight l
      = CD <$> atQ f acc l <*> (atC (atU (atQ f)) $! acc + wl) m <*> (atQ f $! acc + wl + weight m) r
  {-# inline itraverse #-}

type instance IxValue (Seq a) = a
type instance Index (Seq a) = Int

instance Ixed (Seq a) where
  ix i f (Seq q) = case wsplit i q of
    (l,r) -> _head (_One f) r <&> \r' -> Seq (mappend l r')
  {-# inline ix #-}
