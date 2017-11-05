{-# language GADTs #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language RoleAnnotations #-}
{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# options_ghc -Wno-incomplete-patterns -O0 #-}

--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------

module Coda.Relative.Map
  ( Map
  , singleton
  , insert
  , lookup
  , toAscList
  , union
  , split
  , irfoldr
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta hiding (delta)
import Coda.Util.BitQueue
import Control.Lens
import Data.Default
import Data.Hashable
import Data.Function (on)
import Data.Monoid
import GHC.Exts
import Prelude hiding (lookup)

type Size = Int

data Map k a
  = Bin {-# unpack #-} !Size {-# unpack #-} !Delta !k !a !(Map k a) !(Map k a)
  | Tip
  deriving Show

type role Map nominal nominal

instance Relative (Map k a) where
  rel _ Tip = Tip
  rel 0 m   = m -- improve sharing
  rel d (Bin s d' k a l r) = Bin s (d+d') k a l r

size :: Map k a -> Int
size (Bin s _ _ _ _ _) = s
size Tip = 0

irfoldr :: (Delta -> k -> a -> r -> r) -> r -> Delta -> Map k a -> r
irfoldr _ z !_ Tip = z
irfoldr f z d (Bin _ d' k a l r) | !d'' <- d <> d' = irfoldr f (f d'' k a (irfoldr f z d'' r)) d'' l

toAscList :: (StrictRelativeOrder k, Relative a) => Map k a -> [(k,a)]
toAscList = irfoldr (\d k x xs -> (rel d k,rel d x):xs) [] 0

instance (StrictRelativeOrder k, Relative a, Eq a) => Eq (Map k a) where (==) = on (==) toAscList
instance (StrictRelativeOrder k, Relative a, Ord a) => Ord (Map k a) where compare = on compare toAscList
instance (StrictRelativeOrder k, RelativeOrder a) => RelativeOrder (Map k a)
instance (StrictRelativeOrder k, StrictRelativeOrder a) => StrictRelativeOrder (Map k a)

--instance (StrictRelativeOrder k, Show k, Relative a, Show a) => Show (Map k a) where
--  showsPrec d m = showParen (d > 10) $ showString "fromList " . showsPrec 11 (toAscList m)

instance (StrictRelativeOrder k, Hashable k, Relative a, Hashable a) => Hashable (Map k a) where
  hashWithSalt d = hashWithSalt d . toAscList

type instance IxValue (Map k a) = a
type instance Index (Map k a) = k

instance (StrictRelativeOrder k, Relative a) => Ixed (Map k a)
instance (StrictRelativeOrder k, Relative a) => At (Map k a) where
  at !k f m = case lookupTrace k m of
    TraceResult mv d q -> f mv <&> \case
      Nothing -> case mv of
        Nothing -> m
        Just old -> deleteAlong old q m
      Just !new | !nd <- negate d -> case mv of
        Nothing -> insertAlong q (rel nd k) (rel nd new) m
        Just _  -> replaceAlong q (rel nd new) m
  {-# inlinable at #-}

singleton :: k -> a -> Map k a
singleton k a = Bin 1 mempty k a Tip Tip
{-# inline singleton #-}

instance Default (Map k a) where
  def = Tip

-- | /O(m*log(n\/m + 1)), m <= n/
instance (StrictRelativeOrder k, Relative a) => Monoid (Map k a) where
  mempty = Tip
  mappend = union

instance (StrictRelativeOrder k, Relative a) => RelativeMonoid (Map k a)

lookup :: (StrictRelativeOrder k, Relative a) => k -> Map k a -> Maybe a
lookup = go 0 where
  go !_ !_ Tip = Nothing
  go d k (Bin _ d' kx x l r) | !d'' <- d+d' = case compare k (rel d'' kx) of
    LT -> go d'' k l
    GT -> go d'' k r
    EQ -> Just (rel d'' x)
{-# inlinable lookup #-}

union :: (StrictRelativeOrder k, Relative a) => Map k a -> Map k a -> Map k a
union t1 Tip  = t1
union t1 (Bin _ d k x Tip Tip) = insertRD 0 (rel d k) (rel d x) t1
union (Bin _ d k x Tip Tip) t2 = insertD 0 (rel d k) (rel d x) t2
union Tip t2 = t2
union t1@(Bin _ d1 k1 x1 l1 r1) t2 = case split d1 k1 t2 of
  (l2, r2)
    | ptrEq l1l2 l1 && ptrEq r1r2 r1 -> t1
    | otherwise -> link d1 k1 x1 l1l2 r1r2
    where
      !l1l2 = union l1 l2
      !r1r2 = union r1 r2
{-# inlinable union #-}

insert :: StrictRelativeOrder k => k -> a -> Map k a -> Map k a
insert = insertD 0 

insertD :: (Ord k, Relative k) => Delta -> k -> a -> Map k a -> Map k a
insertD !d !kx !x Tip = Bin 1 (negate d) kx x Tip Tip
insertD d kx x t@(Bin sz dy ky y l r) | !d'' <- d+dy = case compare kx (rel d'' ky) of
  LT | ptrEq l' l -> t
     | otherwise -> balanceL dy ky y l' r
     where !l' = insertD d'' kx x l
  GT | ptrEq r' r -> t
     | otherwise -> balanceR dy ky y l r'
     where !r' = insertD d'' kx x r
  EQ -> Bin sz (negate d) kx x (rel d'' l) (rel d'' r)
{-# inlinable insert #-}

insertRD :: (Ord k, Relative k) => Delta -> k -> a -> Map k a -> Map k a
insertRD !d !kx !x Tip = Bin 1 (-d) kx x Tip Tip
insertRD d kx x t@(Bin _ dy ky y l r) | !d'' <- d+dy = case compare kx (rel d'' ky) of
  LT | ptrEq l' l -> t
     | otherwise -> balanceL dy ky y l' r
     where !l' = insertRD d'' kx x l
  GT | ptrEq r' r -> t
     | otherwise -> balanceR dy ky y l r'
     where !r' = insertRD d'' kx x r
  EQ -> t

split :: StrictRelativeOrder k => Delta -> k -> Map k a -> (Map k a, Map k a)
split !d0 !k0 t0 = toPair $ go d0 k0 t0 where
  go :: (Relative k, Ord k) => Delta -> k -> Map k a -> StrictPair (Map k a) (Map k a)
  go d k t = case t of
    Tip -> Tip :*: Tip
    Bin _ dx kx x l r -> case compare (rel (d-dx) k) kx of
      LT -> let lt :*: gt = go (d-dx) k l in lt :*: link dx kx x gt r
      GT -> let lt :*: gt = go (d-dx) k r in link dx kx x l lt :*: gt
      EQ -> rel dx l :*: rel dx r
{-# INLINABLE split #-}

--------------------------------------------------------------------------------
-- Implementation Details
--------------------------------------------------------------------------------

ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# inline ptrEq #-}


toPair :: StrictPair a b -> (a, b)
toPair (a :*: b) = (a, b)
{-# inline toPair #-}

link :: Delta -> k -> a -> Map k a -> Map k a -> Map k a
link d kx x Tip r  = insertMin d kx x r
link d kx x l Tip  = insertMax d kx x l
link d kx x l@(Bin sizeL dy ky y ly ry) r@(Bin sizeR dz kz z lz rz)
  | delta*sizeL < sizeR  = balanceL (d+dz) kz z (link (negate dz) kx x l (rel dz lz)) rz
  | delta*sizeR < sizeL  = balanceR (d+dy) ky y ly (link (negate dy) kx x (rel dy ry) r)
  | otherwise            = bin d kx x l r

bin :: Delta -> k -> a -> Map k a -> Map k a -> Map k a
bin d kx x l r = Bin (size l + size r + 1) d kx x l r

insertMax,insertMin :: Delta -> k -> a -> Map k a -> Map k a
insertMax d kx x t = case t of
  Tip -> Bin 1 (negate d) kx x Tip Tip
  Bin _ dy ky y l r -> balanceR dy ky y l (insertMax (d+dy) kx x r)

insertMin d kx x t = case t of
  Tip -> Bin 1 (negate d) kx x Tip Tip
  Bin _ dy ky y l r -> balanceL dy ky y (insertMin (d+dy) kx x l) r


data StrictPair a b = !a :*: !b

data TraceResult a = TraceResult !(Maybe a) {-# unpack #-} !Delta {-# unpack #-} !BitQueue

-- Look up a key and return a result indicating whether it was found and what path was taken.
lookupTrace :: (Ord k, Relative k) => k -> Map k a -> TraceResult a
lookupTrace = go mempty emptyQB where
  go :: (Ord k, Relative k) => Delta -> BitQueueB -> k -> Map k a -> TraceResult a
  go !d !q !_ Tip = TraceResult Nothing d (buildQ q)
  go d q k (Bin _ d' kx x l r) | !d'' <- d+d' = case compare k (rel d'' kx) of
    LT -> go d'' (snocQB q False) k l
    GT -> go d'' (snocQB q True) k r
    EQ -> TraceResult (Just x) d'' (buildQ q)
{-# inlinable lookupTrace #-}

insertAlong :: BitQueue -> k -> a -> Map k a -> Map k a
insertAlong !_ kx x Tip = singleton kx x
insertAlong q kx x (Bin _ d' ky y l r) = case unconsQ q of
  Just (False, tl) -> balanceL d' ky y (insertAlong tl kx x l) r
  Just (True,tl) -> balanceR d' ky y l (insertAlong tl kx x r)
  Nothing -> error "Coda.Relative.Map.insertAlong: failure"

deleteAlong :: any -> BitQueue -> Map k a -> Map k a
deleteAlong old !q0 !m = go (bogus old) q0 m where
  go :: Proxy# () -> BitQueue -> Map k a -> Map k a
  go !_ !_ Tip = Tip
  go foom q (Bin _ d' ky y l r) = case unconsQ q of
    Just (False, tl) -> balanceR d' ky y (go foom tl l) r
    Just (True, tl) -> balanceL d' ky y l (go foom tl r)
    Nothing -> glue l r

bogus :: a -> Proxy# ()
bogus _ = proxy#
{-# noinline bogus #-}

replaceAlong :: BitQueue -> a -> Map k a -> Map k a
replaceAlong !_ _ Tip = error "Coda.Relative.Map.replaceAlong: failure"
replaceAlong q x (Bin sz d' ky y l r) = case unconsQ q of
  Just (False, tl) -> Bin sz d' ky y (replaceAlong tl x l) r
  Just (True,tl) -> Bin sz d' ky y l (replaceAlong tl x r)
  Nothing -> Bin sz d' ky x l r

balanceL :: Delta -> k -> a -> Map k a -> Map k a -> Map k a
balanceL d k x l r = case r of
  Tip -> case l of
    Tip -> Bin 1 d k x Tip Tip
    Bin _ _ _ _ Tip Tip -> Bin 2 d k x l Tip
    Bin _ ld lk lx Tip (Bin _ lrd lrk lrx _ _) -> Bin 3 (d+ld+lrd) lrk lrx (Bin 1 (-lrd) lk lx Tip Tip) (Bin 1 (-ld-lrd) k x Tip Tip)
    Bin _ ld lk lx ll@Bin{} Tip -> Bin 3 (d+ld) lk lx ll (Bin 1 (-ld) k x Tip Tip)
    Bin ls ld lk lx ll@(Bin lls _ _ _ _ _) lr@(Bin lrs lrd lrk lrx lrl lrr)
      | lrs < ratio*lls -> Bin (1+ls) (d+ld) lk lx ll (Bin (1+lrs) (-ld) k x (rel ld lr) Tip)
      | otherwise -> Bin (1+ls) (d+ld+lrd) lrk lrx 
        (Bin (1+lls+size lrl) (-lrd) lk lx ll (rel lrd lrl))
        (Bin (1+size lrr) (-lrd-ld) k x (rel (ld+lrd) lrr) Tip)

  Bin rs _ _ _ _ _ -> case l of
    Tip -> Bin (1+rs) d k x Tip r
    Bin ls ld lk lx ll lr
      | ls > delta*rs  -> case (ll, lr) of
        (Bin lls _ _ _ _ _, Bin lrs lrd lrk lrx lrl lrr)
          | lrs < ratio*lls -> Bin (1+ls+rs) (d+ld) lk lx ll (Bin (1+rs+lrs) (-ld) k x (rel ld lr) r)
          | otherwise -> Bin (1+ls+rs) (d+ld+lrd) lrk lrx 
            (Bin (1+lls+size lrl) (-lrd) lk lx ll (rel lrd lrl))
            (Bin (1+rs+size lrr) (-lrd-ld) k x (rel (lrd+ld) lrr) r)
        (_, _) -> error "Coda.Relative.Map.balanceL: failure"
      | otherwise -> Bin (1+ls+rs) d k x l r
{-# noinline balanceL #-}

balanceR :: Delta -> k -> a -> Map k a -> Map k a -> Map k a
balanceR d k x l r = case l of
  Tip -> case r of
    Tip -> Bin 1 d k x Tip Tip
    Bin _ _ _ _ Tip Tip -> Bin 2 d k x Tip r
    Bin _ rd rk rx Tip rr@Bin{} -> Bin 3 (d+rd) rk rx (Bin 1 (-rd) k x Tip Tip) rr
    Bin _ rd rk rx (Bin _ rld rlk rlx _ _) Tip -> Bin 3 (d+rd+rld) rlk rlx (Bin 1 (-rld-rd) k x Tip Tip) (Bin 1 (-rld) rk rx Tip Tip)
    Bin rs rd rk rx rl@(Bin rls rld rlk rlx rll rlr) rr@(Bin rrs _ _ _ _ _)
      | rls < ratio*rrs -> Bin (1+rs) (d+rd) rk rx (Bin (1+rls) (-rd) k x Tip (rel rd rl)) rr
      | otherwise -> Bin (1+rs) (d+rd+rld) rlk rlx
        (Bin (1+size rll) (-rld-rd) k x Tip (rel (rd+rld) rll))
        (Bin (1+rrs+size rlr) (-rld) rk rx (rel rld rlr) rr)

  Bin ls _ _ _ _ _ -> case r of
    Tip -> Bin (1+ls) d k x l Tip
    Bin rs rd rk rx rl rr
      | rs > delta*ls  -> case (rl, rr) of
        (Bin rls rld rlk rlx rll rlr, Bin rrs _ _ _ _ _)
          | rls < ratio*rrs -> Bin (1+ls+rs) (d+rd) rk rx (Bin (1+ls+rls) (-rd) k x l (rel rd rl)) rr
          | otherwise -> Bin (1+ls+rs) (d+rd+rld) rlk rlx
            (Bin (1+ls+size rll) (-rd-rld) k x l (rel (rd+rld) rll))
            (Bin (1+rrs+size rlr) (-rld) rk rx (rel rld rlr) rr)
        (_, _) -> error "Coda.Relative.Map.balanceR: failure"
      | otherwise -> Bin (1+ls+rs) d k x l r
{-# NOINLINE balanceR #-}

data MinView k a = MinView !Delta !k !a !(Map k a)
data MaxView k a = MaxView !Delta !k !a !(Map k a)

glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue (Bin sl dl kl xl ll lr) (Bin sr dr kr xr rl rr)
  | sl > sr   = let !(MaxView dm km m l') = maxViewSure dl kl xl ll lr in balanceR dm km m l' (Bin sr (dr-dm) kr xr rl rr)
  | otherwise = let !(MinView dm km m r') = minViewSure dr kr xr rl rr in balanceL dm km m (Bin sl (dl-dm) kl xl ll lr) r'

minViewSure :: Delta -> k -> a -> Map k a -> Map k a -> MinView k a
minViewSure = go where
  go !d k x Tip r = MinView d k x r
  go d k x (Bin _ dl kl xl ll lr) r = case go (d+dl) kl xl ll lr of
    MinView dm km xm l' -> MinView dm km xm (balanceR (-dm) k x (rel dm l') r)

maxViewSure :: Delta -> k -> a -> Map k a -> Map k a -> MaxView k a
maxViewSure = go where
  go !d k x l Tip = MaxView d k x l
  go d k x l (Bin _ dr kr xr rl rr) = case go (d+dr) kr xr rl rr of
    MaxView dm km xm r' -> MaxView dm km xm (balanceL (-dm) k x l (rel dm r'))

delta,ratio :: Int
delta = 3
ratio = 2
