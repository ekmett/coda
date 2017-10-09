{-# language CPP #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language DeriveTraversable #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleContexts #-}
{-# language AutoDeriveTypeable #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}
{-# language ExplicitNamespaces #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Coda.FingerTree
-- Copyright   :  (c) Edward Kmett 2017, (c) Ross Paterson, Ralf Hinze 2006
-- License     :  BSD-style
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-----------------------------------------------------------------------------

module Coda.FingerTree
  ( FingerTree(Empty, Singleton , (:<), (:>))
  , Measured(..)
  -- * Construction
  , empty, singleton
  , fromList
  -- * Deconstruction
  , null
  -- ** Examining the ends
  -- ** Search
  , SearchResult(..), search
  -- ** Splitting
  -- | These functions are special cases of 'search'.
  , split, takeUntil, dropUntil
  -- * Transformation
  , reverse
  -- ** Maps
  , fmap', fmapWithPos, fmapWithContext, unsafeFmap
  -- ** Traversals
  , traverse', traverseWithPos, traverseWithContext, unsafeTraverse
  -- * Example
  -- $example
  ) where

import Prelude hiding (null, reverse)
import qualified Prelude (null)
import Control.Lens hiding (deep)
import Data.Semigroup
import qualified Data.Foldable as Foldable
import GHC.Exts

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
instance Measured a => Semigroup (FingerTree a) where
  (<>) = appendFingerTree0

instance Measured a => Monoid (FingerTree a) where
  mempty = empty
  mappend = (<>)

data Digit a
  = One a
  | Two a a
  | Three a a a
  | Four a a a a
  deriving (Functor,Foldable,Traversable,Show)

-------------------
-- 4.1 Measurements
-------------------

-- | Things that can be measured.
class Monoid (Measure a) => Measured a where
  type Measure a :: *
  measure :: a -> Measure a

instance Measured a => Measured (Digit a) where
  type Measure (Digit a) = Measure a
  measure = foldMap measure

---------------------------
-- 4.2 Caching measurements
---------------------------

data Node a
  = Node2 !(Measure a) a a
  | Node3 !(Measure a) a a a

deriving instance (Show (Measure a), Show a) => Show (Node a)

instance Foldable Node where
  foldMap f (Node2 _ a b) = f a `mappend` f b
  foldMap f (Node3 _ a b c) = f a `mappend` f b `mappend` f c

pattern N2 :: Measured a => a -> a -> Node a
pattern N2 a b <- Node2 _ a b where
  N2 a b = Node2 (measure a `mappend` measure b) a b

pattern N3 :: Measured a => a -> a -> a -> Node a
pattern N3 a b c <- Node3 _ a b c where
  N3 a b c = Node3 (measure a `mappend` measure b `mappend` measure c) a b c

instance Monoid (Measure a) => Measured (Node a) where
  type Measure (Node a) = Measure a
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

-- | A representation of a sequence of values of type @a@, allowing
-- access to the ends in constant time, and append and split in time
-- logarithmic in the size of the smaller piece.
--
-- The collection is also parameterized by a measure type @v@, which
-- is used to specify a position in the sequence for the 'split' operation.
-- The types of the operations enforce the constraint @'Measured' v a@,
-- which also implies that the type @v@ is determined by @a@.
--
-- A variety of abstract data types can be implemented by using different
-- element types and measurements.

data FingerTree a
  = EmptyTree
  | Singleton a
  | Deep !(Measure a) !(Digit a) (FingerTree (Node a)) !(Digit a)
--  deriving Show

#if __GLASGOW_HASKELL__ >= 802
{-# complete_patterns (Empty|EmptyTree),((:<)|(:>)|(Singleton,Deep)) #-}
#endif

deep :: Measured a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf = Deep ((measure pr `mappend` measure m) `mappend` measure sf) pr m sf

-- | /O(1)/. The cached measure of a tree.
instance Measured a => Measured (FingerTree a) where
  type Measure (FingerTree a) = Measure a
  measure EmptyTree           = mempty
  measure (Singleton x)      = measure x
  measure (Deep v _ _ _)  = v

-- | Elements from left to right.
instance Foldable FingerTree where
  foldMap _ EmptyTree = mempty
  foldMap f (Singleton x) = f x
  foldMap f (Deep _ pr m sf) =
    foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf

  null EmptyTree = True
  null _ = False

instance Eq a => Eq (FingerTree a) where
  xs == ys = Foldable.toList xs == Foldable.toList ys

instance Ord a => Ord (FingerTree a) where
  compare xs ys = compare (Foldable.toList xs) (Foldable.toList ys)

instance Show a => Show (FingerTree a) where
  showsPrec p xs = showParen (p > 10) $ showString "fromList " . shows (Foldable.toList xs)

-- | Like 'fmap', but with constraints on the element types.
fmap' :: Measured b => (a -> b) -> FingerTree a -> FingerTree b
fmap' _ EmptyTree = EmptyTree
fmap' f (Singleton x) = Singleton (f x)
fmap' f (Deep _ pr m sf) = deep (fmap f pr) (fmap' (mapNode f) m) (fmap f sf)

mapNode :: Measured b => (a -> b) -> Node a -> Node b
mapNode f (Node2 _ a b) = N2 (f a) (f b)
mapNode f (Node3 _ a b c) = N3 (f a) (f b) (f c)

-- | Map all elements of the tree with a function that also takes the
-- measure of the prefix of the tree to the left of the element.
fmapWithPos :: (Measured a, Measured b) => (Measure a -> a -> b) -> FingerTree a -> FingerTree b
fmapWithPos f = mapWPFingerTree f mempty

mapWPFingerTree :: (Measured a, Measured b) => (Measure a -> a -> b) -> Measure a -> FingerTree a -> FingerTree b
mapWPFingerTree _ _ EmptyTree = EmptyTree
mapWPFingerTree f v (Singleton x) = Singleton (f v x)
mapWPFingerTree f v (Deep _ pr m sf) = deep (mapWPDigit f v pr) (mapWPFingerTree (mapWPNode f) vpr m) (mapWPDigit f vm sf) where
  vpr = v `mappend`  measure pr
  vm = vpr `mappend`  measure m

mapWPNode :: (Measured a, Measured b) => (Measure a -> a -> b) -> Measure a -> Node a -> Node b
mapWPNode f v (Node2 _ a b) = N2 (f v a) (f va b) where
  va = v `mappend` measure a
mapWPNode f v (Node3 _ a b c) = N3 (f v a) (f va b) (f vab c) where
  va = v `mappend` measure a
  vab = va `mappend` measure b

mapWPDigit :: Measured a => (Measure a -> a -> b) -> Measure a -> Digit a -> Digit b
mapWPDigit f v (One a) = One (f v a)
mapWPDigit f v (Two a b) = Two (f v a) (f va b) where
  va = v `mappend` measure a
mapWPDigit f v (Three a b c) = Three (f v a) (f va b) (f vab c) where
  va = v `mappend` measure a
  vab = va `mappend` measure b
mapWPDigit f v (Four a b c d) = Four (f v a) (f va b) (f vab c) (f vabc d) where
  va = v `mappend` measure a
  vab = va `mappend` measure b
  vabc = vab `mappend` measure c

-- | Map all elements of the tree with a function that also takes the
-- measure of the prefix to the left and of the suffix to the right of
-- the element.
fmapWithContext :: (Measured a, Measured b) => (Measure a -> a -> Measure a -> b) -> FingerTree a -> FingerTree b
fmapWithContext f t = mapWCFingerTree f mempty t mempty

mapWCFingerTree :: (Measured a, Measured b) => (Measure a -> a -> Measure a -> b) -> Measure a -> FingerTree a -> Measure a -> FingerTree b
mapWCFingerTree _ _ EmptyTree _ = EmptyTree
mapWCFingerTree f vl (Singleton x) vr = Singleton (f vl x vr)
mapWCFingerTree f vl (Deep _ pr m sf) vr = deep (mapWCDigit f vl pr vmsr) (mapWCFingerTree (mapWCNode f) vlp m vsr) (mapWCDigit f vlpm sf vr) where
  vlp = vl `mappend` measure pr
  vlpm = vlp `mappend` vm
  vmsr = vm `mappend` vsr
  vsr = measure sf `mappend` vr
  vm = measure m

mapWCNode :: (Measured a, Measured b) => (Measure a -> a -> Measure a-> b) -> Measure a -> Node a -> Measure a -> Node b
mapWCNode f vl (Node2 _ a b) vr = N2 (f vl a vb) (f va b vr) where
  va = vl `mappend` measure a
  vb = measure b `mappend` vr
mapWCNode f vl (Node3 _ a b c) vr = N3 (f vl a vbc) (f va b vc) (f vab c vr) where
  va = vl `mappend` measure a
  vab = va `mappend` measure b
  vbc = measure b `mappend` vc
  vc = measure c `mappend` vr

mapWCDigit :: Measured a => (Measure a -> a -> Measure a -> b) -> Measure a -> Digit a -> Measure a -> Digit b
mapWCDigit f vl (One a) vr = One (f vl a vr)
mapWCDigit f vl (Two a b) vr = Two (f vl a vb) (f va b vr) where
  va = vl `mappend` measure a
  vb = measure b `mappend` vr
mapWCDigit f vl (Three a b c) vr = Three (f vl a vbc) (f va b vc) (f vab c vr) where
  va = vl `mappend` measure a
  vab = va `mappend` measure b
  vbc = measure b `mappend` vc
  vc = measure c `mappend` vr
mapWCDigit f vl (Four a b c d) vr = Four (f vl a vbcd) (f va b vcd) (f vab c vd) (f vabc d vr) where
  va = vl `mappend` measure a
  vab = va `mappend` measure b
  vabc = vab `mappend` measure c
  vbcd = measure b `mappend` vcd
  vcd = measure c `mappend` vd
  vd = measure d `mappend` vr

-- | Like 'fmap', but safe only if the function preserves the measure.
unsafeFmap :: (Measure a ~ Measure b) => (a -> b) -> FingerTree a -> FingerTree b
unsafeFmap _ EmptyTree = EmptyTree
unsafeFmap f (Singleton x) = Singleton (f x)
unsafeFmap f (Deep v pr m sf) = Deep v (fmap f pr) (unsafeFmap (unsafeFmapNode f) m) (fmap f sf)

unsafeFmapNode :: (Measure a ~ Measure b) => (a -> b) -> Node a -> Node b
unsafeFmapNode f (Node2 v a b) = Node2 v (f a) (f b)
unsafeFmapNode f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

-- | Like 'traverse', but with constraints on the element types.
traverse' :: (Measured b, Applicative f) => (a -> f b) -> FingerTree a -> f (FingerTree b)
traverse' _ EmptyTree = pure EmptyTree
traverse' f (Singleton x) = Singleton <$> f x
traverse' f (Deep _ pr m sf) = deep <$> traverse f pr <*> traverse' (traverseNode f) m <*> traverse f sf

traverseNode :: (Measured b, Applicative f) => (a -> f b) -> Node a -> f (Node b)
traverseNode f (Node2 _ a b) = N2 <$> f a <*> f b
traverseNode f (Node3 _ a b c) = N3 <$> f a <*> f b <*> f c

-- | Traverse the tree from left to right with a function that also
-- takes the measure of the prefix of the tree to the left of the element.
traverseWithPos :: (Measured a, Measured b, Applicative f) => (Measure a -> a -> f b) -> FingerTree a -> f (FingerTree b)
traverseWithPos f = traverseWPFingerTree f mempty

traverseWPFingerTree :: (Measured a, Measured b, Applicative f) => (Measure a -> a -> f b) -> Measure a -> FingerTree a -> f (FingerTree b)
traverseWPFingerTree _ _ EmptyTree = pure EmptyTree
traverseWPFingerTree f v (Singleton x) = Singleton <$> f v x
traverseWPFingerTree f v (Deep _ pr m sf) = deep <$> traverseWPDigit f v pr <*> traverseWPFingerTree (traverseWPNode f) vpr m <*> traverseWPDigit f vm sf where
  vpr = v `mappend`  measure pr
  vm = vpr `mappend` measure m

traverseWPNode :: (Measured a, Measured b, Applicative f) => (Measure a -> a -> f b) -> Measure a -> Node a -> f (Node b)
traverseWPNode f v (Node2 _ a b) = N2 <$> f v a <*> f va b where
  va = v `mappend` measure a
traverseWPNode f v (Node3 _ a b c) = N3 <$> f v a <*> f va b <*> f vab c where
  va = v `mappend` measure a
  vab = va `mappend` measure b

traverseWPDigit :: (Measured a, Applicative f) => (Measure a -> a -> f b) -> Measure a -> Digit a -> f (Digit b)
traverseWPDigit f v (One a) = One <$> f v a
traverseWPDigit f v (Two a b) = Two <$> f v a <*> f va b where
  va = v `mappend` measure a
traverseWPDigit f v (Three a b c) = Three <$> f v a <*> f va b <*> f vab c where
  va = v `mappend` measure a
  vab = va `mappend` measure b
traverseWPDigit f v (Four a b c d) = Four <$> f v a <*> f va b <*> f vab c <*> f vabc d where
  va = v `mappend` measure a
  vab = va `mappend` measure b
  vabc = vab `mappend` measure c

-- | Traverse the tree from left to right with a function that also
-- takes the measure of the prefix to the left and the measure of the
-- suffix to the right of the element.
traverseWithContext :: (Measured a, Measured b, Applicative f) => (Measure a -> a -> Measure a -> f b) -> FingerTree a -> f (FingerTree b)
traverseWithContext f t = traverseWCFingerTree f mempty t mempty

traverseWCFingerTree :: (Measured a, Measured b, Applicative f) => (Measure a -> a -> Measure a -> f b) -> Measure a -> FingerTree a -> Measure a -> f (FingerTree b)
traverseWCFingerTree _ _ EmptyTree _ = pure EmptyTree
traverseWCFingerTree f vl (Singleton x) vr = Singleton <$> f vl x vr
traverseWCFingerTree f vl (Deep _ pr m sf) vr = deep <$> traverseWCDigit f vl pr vmsr <*> traverseWCFingerTree (traverseWCNode f) vlp m vsr <*> traverseWCDigit f vlpm sf vr where
  vlp = vl `mappend` measure pr
  vlpm = vlp `mappend` vm
  vmsr = vm `mappend` vsr
  vsr = measure sf `mappend` vr
  vm = measure m

traverseWCNode :: (Measured a, Measured b, Applicative f) => (Measure a -> a -> Measure a -> f b) -> Measure a -> Node a -> Measure a -> f (Node b)
traverseWCNode f vl (Node2 _ a b) vr = N2 <$> f vl a vb <*> f va b vr where
  va = vl `mappend` measure a
  vb = measure a `mappend` vr
traverseWCNode f vl (Node3 _ a b c) vr = N3 <$> f vl a vbc <*> f va b vc <*> f vab c vr where
  va = vl `mappend` measure a
  vab = va `mappend` measure b
  vc = measure c `mappend` vr
  vbc = measure b `mappend` vc

traverseWCDigit :: (Measured a, Applicative f) => (Measure a -> a -> Measure a -> f b) -> Measure a -> Digit a -> Measure a -> f (Digit b)
traverseWCDigit f vl (One a) vr = One <$> f vl a vr
traverseWCDigit f vl (Two a b) vr = Two <$> f vl a vb <*> f va b vr where
  va = vl `mappend` measure a
  vb = measure a `mappend` vr
traverseWCDigit f vl (Three a b c) vr = Three <$> f vl a vbc <*> f va b vc <*> f vab c vr where
  va = vl `mappend` measure a
  vab = va `mappend` measure b
  vc = measure c `mappend` vr
  vbc = measure b `mappend` vc
traverseWCDigit f vl (Four a b c d) vr = Four <$> f vl a vbcd <*> f va b vcd <*> f vab c vd <*> f vabc d vr where
  va = vl `mappend` measure a
  vab = va `mappend` measure b
  vabc = vab `mappend` measure c
  vd = measure d `mappend` vr
  vcd = measure c `mappend` vd
  vbcd = measure b `mappend` vcd

-- | Like 'traverse', but safe only if the function preserves the measure.
unsafeTraverse :: (Measure a ~ Measure b, Applicative f) => (a -> f b) -> FingerTree a -> f (FingerTree b)
unsafeTraverse _ EmptyTree = pure EmptyTree
unsafeTraverse f (Singleton x) = Singleton <$> f x
unsafeTraverse f (Deep v pr m sf) = Deep v <$> traverse f pr <*> unsafeTraverse (unsafeTraverseNode f) m <*> traverse f sf

unsafeTraverseNode :: (Measure a ~ Measure b, Applicative f) => (a -> f b) -> Node a -> f (Node b)
unsafeTraverseNode f (Node2 v a b) = Node2 v <$> f a <*> f b
unsafeTraverseNode f (Node3 v a b c) = Node3 v <$> f a <*> f b <*> f c

-----------------------------------------------------
-- 4.3 Construction, deconstruction and concatenation
-----------------------------------------------------

-- | /O(1)/. The empty sequence.
empty :: FingerTree a
empty = EmptyTree

-- | /O(1)/. A singleton sequence.
singleton :: a -> FingerTree a
singleton = Singleton

instance Measured a => IsList (FingerTree a) where
  type Item (FingerTree a) = a

  -- | /O(n)/. Create a sequence from a finite list of elements.
  -- The opposite operation 'toList' is supplied by the 'Foldable' instance.
  fromList = foldr (<|) EmptyTree

  toList = Foldable.toList

instance (Measured a, Measured b) => Cons (FingerTree a) (FingerTree b) a b where
  _Cons = prism kons unkons where
    kons (a, EmptyTree) =  Singleton a
    kons (a, Singleton b) =  deep (One a) EmptyTree (One b)
    kons (a, Deep v (Four b c d e) m sf) = m `seq` Deep (measure a `mappend` v) (Two a b) (N3 c d e <| m) sf
    kons (a, Deep v pr m sf) = Deep (measure a `mappend` v) (consDigit a pr) m sf
    unkons EmptyTree = Left EmptyTree
    unkons (Singleton x) = Right (x, EmptyTree)
    unkons (Deep _ (One x) m sf) = Right (x, rotL m sf)
    unkons (Deep _ pr m sf) = Right (lheadDigit pr,  deep (ltailDigit pr) m sf)

instance (Measured a, Measured b) => Snoc (FingerTree a) (FingerTree b) a b where
  _Snoc = prism snok unsnok where
    snok (EmptyTree, a) =  Singleton a
    snok (Singleton a, b) =  deep (One a) EmptyTree (One b)
    snok (Deep v pr m (Four a b c d), e) = m `seq` Deep (v `mappend` measure e) pr (m |> N3 a b c) (Two d e)
    snok (Deep v pr m sf, x) = Deep (v `mappend` measure x) pr m (snocDigit sf x)
    unsnok EmptyTree = Left EmptyTree
    unsnok (Singleton x) = Right (EmptyTree, x)
    unsnok (Deep _ pr m (One x)) = Right (rotR pr m, x)
    unsnok (Deep _ pr m sf) = Right (deep pr m (rtailDigit sf), rheadDigit sf)

instance AsEmpty (FingerTree a) where
  _Empty = prism (const EmptyTree) $ \case
    EmptyTree -> Right ()
    xs -> Left xs

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d
consDigit _ (Four _ _ _ _) = illegal_argument "consDigit"

snocDigit :: Digit a -> a -> Digit a
snocDigit (One a) b = Two a b
snocDigit (Two a b) c = Three a b c
snocDigit (Three a b c) d = Four a b c d
snocDigit (Four _ _ _ _) _ = illegal_argument "snocDigit"

-- | /O(1)/. Is this the empty sequence?
null :: FingerTree a -> Bool
null EmptyTree = True
null _ = False

rotL :: Measured a => FingerTree (Node a) -> Digit a -> FingerTree a
rotL m sf = case m of
  EmptyTree -> digitToFingerTree sf
  a :< m' -> Deep (measure m `mappend` measure sf) (nodeToDigit a) m' sf
#if __GLASGOW_HASKELL__ < 802
  _ -> illegal_argument "rotL"
#endif

lheadDigit :: Digit a -> a
lheadDigit (One a) = a
lheadDigit (Two a _) = a
lheadDigit (Three a _ _) = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (One _) = illegal_argument "ltailDigit"
ltailDigit (Two _ b) = One b
ltailDigit (Three _ b c) = Two b c
ltailDigit (Four _ b c d) = Three b c d

rotR :: Measured a => Digit a -> FingerTree (Node a) -> FingerTree a
rotR pr m = case m of
  EmptyTree -> digitToFingerTree pr
  m' :> a -> Deep (measure pr `mappend` measure m) pr m' (nodeToDigit a)
#if __GLASGOW_HASKELL__ < 802
  _ -> illegal_argument "rotL"
#endif

rheadDigit :: Digit a -> a
rheadDigit (One a) = a
rheadDigit (Two _ b) = b
rheadDigit (Three _ _ c) = c
rheadDigit (Four _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (One _) = illegal_argument "rtailDigit"
rtailDigit (Two a _) = One a
rtailDigit (Three a b _) = Two a b
rtailDigit (Four a b c _) = Three a b c

digitToFingerTree :: Measured a => Digit a -> FingerTree a
digitToFingerTree (One a) = Singleton a
digitToFingerTree (Two a b) = deep (One a) EmptyTree (One b)
digitToFingerTree (Three a b c) = deep (Two a b) EmptyTree (One c)
digitToFingerTree (Four a b c d) = deep (Two a b) EmptyTree (Two c d)

----------------
-- Concatenation
----------------

appendFingerTree0 :: Measured a => FingerTree a -> FingerTree a -> FingerTree a
appendFingerTree0 EmptyTree xs = xs
appendFingerTree0 xs EmptyTree = xs
appendFingerTree0 (Singleton x) xs = x <| xs
appendFingerTree0 xs (Singleton x) = xs |> x
appendFingerTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2) = deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: Measured a => FingerTree (Node a) -> Digit a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits0 m1 (One a) (One b) m2 = appendFingerTree1 m1 (N2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 = appendFingerTree1 m1 (N3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 = appendFingerTree2 m1 (N2 a b) (N2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 = appendFingerTree1 m1 (N3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 = appendFingerTree2 m1 (N2 a b) (N2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 = appendFingerTree2 m1 (N2 a b) (N2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2

appendFingerTree1 :: Measured a => FingerTree a -> a -> FingerTree a -> FingerTree a
appendFingerTree1 EmptyTree a xs = a <| xs
appendFingerTree1 xs a EmptyTree = xs |> a
appendFingerTree1 (Singleton x) a xs = x <| a <| xs
appendFingerTree1 xs a (Singleton x) = xs |> a |> x
appendFingerTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) = deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: Measured a => FingerTree (Node a) -> Digit a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits1 m1 (One a) b (One c) m2 = appendFingerTree1 m1 (N3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 = appendFingerTree2 m1 (N2 a b) (N2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 = appendFingerTree2 m1 (N2 a b) (N2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2

appendFingerTree2 :: Measured a => FingerTree a -> a -> a -> FingerTree a -> FingerTree a
appendFingerTree2 EmptyTree a b xs = a <| b <| xs
appendFingerTree2 xs a b EmptyTree = xs |> a |> b
appendFingerTree2 (Singleton x) a b xs = x <| a <| b <| xs
appendFingerTree2 xs a b (Singleton x) = xs |> a |> b |> x
appendFingerTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) = deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: Measured a => FingerTree (Node a) -> Digit a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits2 m1 (One a) b c (One d) m2 = appendFingerTree2 m1 (N2 a b) (N2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N2 g h) (N2 i j) m2

appendFingerTree3 :: Measured a => FingerTree a -> a -> a -> a -> FingerTree a -> FingerTree a
appendFingerTree3 EmptyTree a b c xs = a <| b <| c <| xs
appendFingerTree3 xs a b c EmptyTree = xs |> a |> b |> c
appendFingerTree3 (Singleton x) a b c xs = x <| a <| b <| c <| xs
appendFingerTree3 xs a b c (Singleton x) = xs |> a |> b |> c |> x
appendFingerTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) = deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: Measured a => FingerTree (Node a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits3 m1 (One a) b c d (One e) m2 = appendFingerTree2 m1 (N3 a b c) (N2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N2 g h) (N2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N2 g h) (N2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N3 g h i) (N2 j k) m2

appendFingerTree4 :: Measured a => FingerTree a -> a -> a -> a -> a -> FingerTree a -> FingerTree a
appendFingerTree4 EmptyTree a b c d xs = a <| b <| c <| d <| xs
appendFingerTree4 xs a b c d EmptyTree = xs |> a |> b |> c |> d
appendFingerTree4 (Singleton x) a b c d xs = x <| a <| b <| c <| d <| xs
appendFingerTree4 xs a b c d (Singleton x) = xs |> a |> b |> c |> d |> x
appendFingerTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) = deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: Measured a => FingerTree (Node a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits4 m1 (One a) b c d e (One f) m2 = appendFingerTree2 m1 (N3 a b c) (N3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 = appendFingerTree3 m1 (N3 a b c) (N2 d e) (N2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N2 g h) (N2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N2 g h) (N2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N3 g h i) (N2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 = appendFingerTree3 m1 (N3 a b c) (N3 d e f) (N3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N2 g h) (N2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N3 g h i) (N2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 = appendFingerTree4 m1 (N3 a b c) (N3 d e f) (N3 g h i) (N3 j k l) m2

----------------
-- 4.4 Splitting
----------------

-- | A result of 'search', attempting to find a point where a predicate
-- on splits of the sequence changes from 'False' to 'True'.
data SearchResult a
  = Position (FingerTree a) a (FingerTree a)
    -- ^ A tree opened at a particular element: the prefix to the
    -- left, the element, and the suffix to the right.
  | OnLeft
    -- ^ A position to the left of the sequence, indicating that the
    -- predicate is 'True' at both ends.
  | OnRight
    -- ^ A position to the right of the sequence, indicating that the
    -- predicate is 'False' at both ends.
  | Nowhere
    -- ^ No position in the tree, returned if the predicate is 'True'
    -- at the left end and 'False' at the right end.  This will not
    -- occur if the predicate in monotonic on the tree.
  deriving (Eq, Ord, Show)

-- | /O(log(min(i,n-i)))/. Search a sequence for a point where a predicate
-- on splits of the sequence changes from 'False' to 'True'.
--
-- The argument @p@ is a relation between the measures of the two
-- sequences that could be appended together to form the sequence @t@.
-- If the relation is 'False' at the leftmost split and 'True' at the
-- rightmost split, i.e.
--
-- @not (p 'mempty' ('measure' t)) && p ('measure' t) 'mempty'@
--
-- then there must exist an element @x@ in the sequence such that @p@
-- is 'False' for the split immediately before @x@ and 'True' for the
-- split just after it:
--
-- <<images/search.svg>>
--
-- In this situation, @'search' p t@ returns such an element @x@ and the
-- pieces @l@ and @r@ of the sequence to its left and right respectively.
-- That is, it returns @'Position' l x r@ such that
--
-- * @l >< (x <| r) = t@
--
-- * @not (p (measure l) (measure (x <| r))@
--
-- * @p (measure (l |> x)) (measure r)@
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/ on @t@.
search :: Measured a => (Measure a -> Measure a -> Bool) -> FingerTree a -> SearchResult a
search p t
  | p_left && p_right = OnLeft
  | not p_left && p_right = case searchFingerTree p mempty t mempty of
        Split l x r -> Position l x r
  | not p_left && not p_right = OnRight
  | otherwise = Nowhere
  where
    p_left = p mempty vt
    p_right = p vt mempty
    vt = measure t

-- isSplit :: (Measured v a) => (v -> v -> Bool) -> v -> a -> v -> Bool
-- isSplit p vl x vr = not (p vl (v `mappend` vr)) && p (vl `mappend` v) vr
--   where v = measure x
--
-- property:
-- isSplit p vl t vr =>
--    let Split l x r = search t in
--    isSplit p (vl `mappend` measure l) x (measure r `mappend` vr)

searchFingerTree :: Measured a => (Measure a -> Measure a -> Bool) -> Measure a -> FingerTree a -> Measure a -> Split (FingerTree a) a
searchFingerTree _ _ EmptyTree _ = illegal_argument "searchFingerTree"
searchFingerTree _ _ (Singleton x) _ = Split EmptyTree x EmptyTree
searchFingerTree p vl (Deep _ pr m sf) vr
  | p vlp vmsr
  , Split l x r <- searchDigit p vl pr vmsr
  = Split (maybe EmptyTree digitToFingerTree l) x (deepL r m sf)

  | p vlpm vsr
  , Split ml xs mr <- searchFingerTree p vlp m vsr
  , Split l x r <- searchNode p (vlp `mappend` measure ml) xs (measure mr `mappend` vsr)
  = Split (deepR pr  ml l) x (deepL r mr sf)

  | Split l x r <- searchDigit p vm sf vr = Split (deepR pr  m  l) x (maybe EmptyTree digitToFingerTree r)
  where
    vlp = vl `mappend` measure pr
    vlpm = vlp `mappend` vm
    vmsr = vm `mappend` vsr
    vsr = measure sf `mappend` vr
    vm = measure m

searchNode :: Measured a => (Measure a -> Measure a -> Bool) -> Measure a -> Node a -> Measure a -> Split (Maybe (Digit a)) a
searchNode p vl (Node2 _ a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va = vl `mappend` measure a
    vb = measure b `mappend` vr
searchNode p vl (Node3 _ a b c) vr
  | p va vbc = Split Nothing a (Just (Two b c))
  | p vab vc = Split (Just (One a)) b (Just (One c))
  | otherwise = Split (Just (Two a b)) c Nothing
  where
    va = vl `mappend` measure a
    vab = va `mappend` measure b
    vc = measure c `mappend` vr
    vbc = measure b `mappend` vc

searchDigit :: Measured a => (Measure a -> Measure a -> Bool) -> Measure a -> Digit a -> Measure a -> Split (Maybe (Digit a)) a
searchDigit _ vl (One a) vr = vl `seq` vr `seq` Split Nothing a Nothing
searchDigit p vl (Two a b) vr
  | p va vb = Split Nothing a (Just (One b))
  | otherwise = Split (Just (One a)) b Nothing
  where
    va = vl `mappend` measure a
    vb = measure b `mappend` vr
searchDigit p vl (Three a b c) vr
  | p va vbc = Split Nothing a (Just (Two b c))
  | p vab vc = Split (Just (One a)) b (Just (One c))
  | otherwise = Split (Just (Two a b)) c Nothing
  where
    va = vl `mappend` measure a
    vab = va `mappend` measure b
    vbc = measure b `mappend` vc
    vc = measure c `mappend` vr
searchDigit p vl (Four a b c d) vr
  | p va vbcd = Split Nothing a (Just (Three b c d))
  | p vab vcd = Split (Just (One a)) b (Just (Two c d))
  | p vabc vd = Split (Just (Two a b)) c (Just (One d))
  | otherwise = Split (Just (Three a b c)) d Nothing
  where
    va = vl `mappend` measure a
    vab = va `mappend` measure b
    vabc = vab `mappend` measure c
    vbcd = measure b `mappend` vcd
    vcd = measure c `mappend` vd
    vd = measure d `mappend` vr

-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated measure of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
split ::  Measured a => (Measure a -> Bool) -> FingerTree a -> (FingerTree a, FingerTree a)
split _ EmptyTree = (EmptyTree, EmptyTree)
split p xs
  | p (measure xs) =  (l, x <| r)
  | otherwise   =  (xs, EmptyTree)
  where
    Split l x r = splitFingerTree p mempty xs

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'takeUntil' p t@ is the largest
-- prefix of @t@ whose measure does not satisfy @p@.
--
-- *  @'takeUntil' p t = 'fst' ('split' p t)@
takeUntil :: Measured a => (Measure a -> Bool) -> FingerTree a -> FingerTree a
takeUntil p  =  fst . split p

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'dropUntil' p t@ is the rest of @t@
-- after removing the largest prefix whose measure does not satisfy @p@.
--
-- * @'dropUntil' p t = 'snd' ('split' p t)@
dropUntil :: Measured a => (Measure a -> Bool) -> FingerTree a -> FingerTree a
dropUntil p  =  snd . split p

data Split t a = Split t a t

splitFingerTree :: Measured a => (Measure a -> Bool) -> Measure a -> FingerTree a -> Split (FingerTree a) a
splitFingerTree _ _ EmptyTree = illegal_argument "splitFingerTree"
splitFingerTree _ _ (Singleton x) = Split EmptyTree x EmptyTree
splitFingerTree p i (Deep _ pr m sf)
  | p vpr
  , Split l x r <- splitDigit p i pr
  = Split (maybe EmptyTree digitToFingerTree l) x (deepL r m sf)

  | p vm
  , Split ml xs mr <- splitFingerTree p vpr m
  , Split l x r <- splitNode p (vpr `mappend` measure ml) xs
  = Split (deepR pr  ml l) x (deepL r mr sf)

  | Split l x r <- splitDigit p vm sf
  = Split (deepR pr  m  l) x (maybe EmptyTree digitToFingerTree r)
  where
    vpr = i `mappend` measure pr
    vm = vpr `mappend` measure m

deepL :: Measured a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf = rotL m sf
deepL (Just pr) m sf = deep pr m sf

deepR :: Measured a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing = rotR pr m
deepR pr m (Just sf) = deep pr m sf

splitNode :: Measured a => (Measure a -> Bool) -> Measure a -> Node a -> Split (Maybe (Digit a)) a
splitNode p i (Node2 _ a b)
  | p va        = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va = i `mappend` measure a
splitNode p i (Node3 _ a b c)
  | p va = Split Nothing a (Just (Two b c))
  | p vab = Split (Just (One a)) b (Just (One c))
  | otherwise = Split (Just (Two a b)) c Nothing
  where
    va = i `mappend` measure a
    vab = va `mappend` measure b

splitDigit :: Measured a => (Measure a -> Bool) -> Measure a -> Digit a -> Split (Maybe (Digit a)) a
splitDigit _ i (One a) = i `seq` Split Nothing a Nothing
splitDigit p i (Two a b)
  | p va = Split Nothing a (Just (One b))
  | otherwise = Split (Just (One a)) b Nothing
  where
    va = i `mappend` measure a
splitDigit p i (Three a b c)
  | p va = Split Nothing a (Just (Two b c))
  | p vab = Split (Just (One a)) b (Just (One c))
  | otherwise = Split (Just (Two a b)) c Nothing
  where
    va = i `mappend` measure a
    vab = va `mappend` measure b
splitDigit p i (Four a b c d)
  | p va = Split Nothing a (Just (Three b c d))
  | p vab = Split (Just (One a)) b (Just (Two c d))
  | p vabc = Split (Just (Two a b)) c (Just (One d))
  | otherwise = Split (Just (Three a b c)) d Nothing
  where
    va = i `mappend` measure a
    vab = va `mappend` measure b
    vabc = vab `mappend` measure c

------------------
-- Transformations
------------------

-- | /O(n)/. The reverse of a sequence.
reverse :: Measured a => FingerTree a -> FingerTree a
reverse = reverseFingerTree id

reverseFingerTree :: Measured b => (a -> b) -> FingerTree a -> FingerTree b
reverseFingerTree _ EmptyTree = EmptyTree
reverseFingerTree f (Singleton x) = Singleton (f x)
reverseFingerTree f (Deep _ pr m sf) = deep (reverseDigit f sf) (reverseFingerTree (reverseNode f) m) (reverseDigit f pr)

reverseNode :: Measured b => (a -> b) -> Node a -> Node b
reverseNode f (Node2 _ a b) = N2 (f b) (f a)
reverseNode f (Node3 _ a b c) = N3 (f c) (f b) (f a)

reverseDigit :: (a -> b) -> Digit a -> Digit b
reverseDigit f (One a) = One (f a)
reverseDigit f (Two a b) = Two (f b) (f a)
reverseDigit f (Three a b c) = Three (f c) (f b) (f a)
reverseDigit f (Four a b c d) = Four (f d) (f c) (f b) (f a)

illegal_argument :: String -> a
illegal_argument name = error $ "Logic error: " ++ name ++ " called with illegal argument"
