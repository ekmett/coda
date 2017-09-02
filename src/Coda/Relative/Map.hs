{-# language GADTs #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
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
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta hiding (delta)
import Coda.Relative.Foldable
import Coda.Util.BitQueue
import Control.Lens hiding (lazy)
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

type role Map nominal nominal

pattern Bin' :: (Relative k, Relative a) => Size -> k -> a -> Map k a -> Map k a -> Map k a
pattern Bin' s k a l r <- Bin s d (rel d -> k) (rel d -> a) (rel d -> l) (rel d -> r) where
  Bin' s k a l r = Bin s 0 k a l r

instance Relative (Map k a) where
  rel _ Tip = Tip
  rel 0 m   = m -- improve sharing
  rel d (Bin s d' k a l r) = Bin s (d <> d') k a l r

-- | A Map with /O(1)/ rel, /O(log n)/ insert/lookup/delete
--
-- Uses @lens@ and 'Foldable' for the bulk of the API
instance RelativeFoldable (Map k) where
  rfoldMap !d !f (Bin _ d' _ a l r) | !d'' <- d <> d' = rfoldMap d'' f l <> f (rel d'' a) <> rfoldMap d'' f r
  rfoldMap _ _ Tip = mempty

  rfoldr !_ _ z Tip = z
  rfoldr d f z (Bin _ d' _ a l r) | !d'' <- d <> d' = rfoldr d'' f (f (rel d'' a) (rfoldr d'' f z r)) l

  rnull Tip = True
  rnull _ = False

  rlength Tip = 0
  rlength (Bin s _ _ _ _ _) = s

size :: Map k a -> Int
size = rlength

instance StrictRelativeOrder k => RelativeFoldableWithIndex k (Map k) where
  irfoldMap = go where
    go !_ _ Tip = mempty
    go !d f (Bin _ d' k a l r) | !d'' <- d <> d' = go d'' f l <> f (rel d'' k) (rel d'' a) <> go d'' f r

  irfoldr = go where
    go !_ _ z Tip = z
    go d f z (Bin _ d' k a l r) | !d'' <- d <> d' = go d'' f (f (rel d'' k) (rel d'' a) (go d'' f z r)) l

toAscList :: (StrictRelativeOrder k, Relative a) => Map k a -> [(k,a)]
toAscList = irfoldr 0 (\k x xs -> (k,x):xs) []

instance (StrictRelativeOrder k, Relative a, Eq a) => Eq (Map k a) where (==) = on (==) toAscList
instance (StrictRelativeOrder k, Relative a, Ord a) => Ord (Map k a) where compare = on compare toAscList
instance (StrictRelativeOrder k, RelativeOrder a) => RelativeOrder (Map k a)
instance (StrictRelativeOrder k, StrictRelativeOrder a) => StrictRelativeOrder (Map k a)

instance (StrictRelativeOrder k, Show k, Relative a, Show a) => Show (Map k a) where
  showsPrec d m = showParen (d > 10) $ showString "fromList " . showsPrec 11 (toAscList m)

instance (StrictRelativeOrder k, Hashable k, Relative a, Hashable a) => Hashable (Map k a) where
  hashWithSalt d = hashWithSalt d . toAscList

type instance IxValue (Map k a) = a
type instance Index (Map k a) = k

instance (StrictRelativeOrder k, Relative a) => Ixed (Map k a)
instance (StrictRelativeOrder k, Relative a) => At (Map k a) where
  at !k f m = case lookupTrace k m of
    TraceResult mv q -> f mv <&> \case
      Nothing -> case mv of
        Nothing -> m
        Just old -> deleteAlong old q mempty m
      Just !new -> case mv of
        Nothing -> insertAlong q mempty k new m
        Just _  -> replaceAlong q mempty new m
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
  go d k (Bin _ d' kx x l r) | !d'' <- d <> d' = case compare k (rel d'' kx) of
    LT -> go d'' k l
    GT -> go d'' k r
    EQ -> Just (rel d'' x)
{-# inlinable lookup #-}

union :: (StrictRelativeOrder k, Relative a) => Map k a -> Map k a -> Map k a
union t1 Tip  = t1
union t1 (Bin' _ k x Tip Tip) = insertR k x t1
union (Bin' _ k x Tip Tip) t2 = insert k x t2
union Tip t2 = t2
union t1@(Bin' _ k1 x1 l1 r1) t2 = case split k1 t2 of
  (l2, r2) | l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1 -> t1
           | otherwise -> link k1 x1 l1l2 r1r2
           where !l1l2 = union l1 l2
                 !r1r2 = union r1 r2
{-# inlinable union #-}

insert :: (StrictRelativeOrder k, Relative a) => k -> a -> Map k a -> Map k a
insert kx0 = go kx0 kx0 where
  go :: (Ord k, Relative k, Relative a) => k -> k -> a -> Map k a -> Map k a
  go orig !_  x Tip = singleton (lazy orig) x
  go orig !kx x t@(Bin' sz ky y l r) = case compare kx ky of
    LT | l' `ptrEq` l -> t
       | otherwise -> balanceL ky y l' r
       where !l' = go orig kx x l
    GT | r' `ptrEq` r -> t
       | otherwise -> balanceR ky y l r'
       where !r' = go orig kx x r
    EQ | x `ptrEq` y && (lazy orig `seq` (orig `ptrEq` ky)) -> t
       | otherwise -> Bin' sz (lazy orig) x l r
{-# inlinable insert #-}

split :: (StrictRelativeOrder k, Relative a) => k -> Map k a -> (Map k a, Map k a)
split !k0 t0 = toPair $ go k0 t0 where
  go k t = case t of
    Tip -> Tip :*: Tip
    Bin' _ kx x l r -> case compare k kx of
      LT -> let (lt :*: gt) = go k l in lt :*: link kx x gt r
      GT -> let (lt :*: gt) = go k r in link kx x l lt :*: gt
      EQ -> (l :*: r)
{-# INLINABLE split #-}

--------------------------------------------------------------------------------
-- Implementation Details
--------------------------------------------------------------------------------


ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# inline ptrEq #-}

insertR :: (StrictRelativeOrder k, Relative a) => k -> a -> Map k a -> Map k a
insertR kx0 = go kx0 kx0 where
  go :: (Ord k, Relative k, Relative a) => k -> k -> a -> Map k a -> Map k a
  go orig !_  x Tip = singleton (lazy orig) x
  go orig !kx x t@(Bin' _ ky y l r) = case compare kx ky of
    LT | l' `ptrEq` l -> t
       | otherwise -> balanceL ky y l' r
       where !l' = go orig kx x l
    GT | r' `ptrEq` r -> t
       | otherwise -> balanceR ky y l r'
       where !r' = go orig kx x r
    EQ -> t

toPair :: StrictPair a b -> (a, b)
toPair (a :*: b) = (a, b)
{-# inline toPair #-}

link :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> Map k a
link kx x Tip r  = insertMin kx x r
link kx x l Tip  = insertMax kx x l
link kx x l@(Bin sizeL dy ky y ly ry) r@(Bin sizeR dz kz z lz rz)
  | delta*sizeL < sizeR  = balanceL (rel dz kz) (rel dz z) (link kx x l (rel dz lz)) (rel dz rz)
  | delta*sizeR < sizeL  = balanceR (rel dy ky) (rel dy y) (rel dy ly) (link kx x (rel dy ry) r)
  | otherwise            = bin kx x l r

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin kx x l r = Bin (size l + size r + 1) 0 kx x l r

insertMax,insertMin :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a
insertMax kx x t = case t of
  Tip -> singleton kx x
  Bin' _ ky y l r -> balanceR ky y l (insertMax kx x r)

insertMin kx x t = case t of
  Tip -> singleton kx x
  Bin' _ ky y l r -> balanceL ky y (insertMin kx x l) r


data StrictPair a b = !a :*: !b

data TraceResult a = TraceResult !(Maybe a) {-# unpack #-} !BitQueue

-- Look up a key and return a result indicating whether it was found
-- and what path was taken.
lookupTrace :: (StrictRelativeOrder k, Relative a) => k -> Map k a -> TraceResult a
lookupTrace = go mempty emptyQB where
  go :: (Ord k, Relative k, Relative a) => Delta -> BitQueueB -> k -> Map k a -> TraceResult a
  go !_ !q !_ Tip = TraceResult Nothing (buildQ q)
  go d q k (Bin _ d' kx x l r) | !d'' <- d <> d' = case compare k (rel d'' kx) of
    LT -> go d'' (snocQB q False) k l
    GT -> go d'' (snocQB q True) k r
    EQ -> TraceResult (Just $! rel d'' x) (buildQ q)
{-# inlinable lookupTrace #-}

insertAlong :: (Relative k, Relative a) => BitQueue -> Delta -> k -> a -> Map k a -> Map k a
insertAlong !_ d kx x Tip = singleton (rel d kx) (rel d x)
insertAlong q d kx x (Bin _ d' ky y l r) | !d'' <- d <> d' = case unconsQ q of
  Just (False, tl) -> balanceL (rel d'' ky) (rel d'' y) (insertAlong tl d'' kx x l) (rel d'' r)
  Just (True,tl) -> balanceR (rel d'' ky) (rel d'' y) (rel d'' l) (insertAlong tl d'' kx x r)
  Nothing -> error "Coda.Relative.Map.insertAlong: failure"

deleteAlong :: (Relative k, Relative a) => any -> BitQueue -> Delta -> Map k a -> Map k a
deleteAlong old !q0 !d0 !m = go (bogus old) q0 d0 m where
  go :: (Relative k, Relative a) => Proxy# () -> BitQueue -> Delta -> Map k a -> Map k a
  go !_ !_ !_ Tip = Tip
  go foom q d (Bin _ d' ky y l r) | !d'' <- d <> d' = case unconsQ q of
    Just (False, tl) -> balanceR (rel d'' ky) (rel d'' y) (go foom tl d'' l) (rel d'' r)
    Just (True, tl) -> balanceL (rel d'' ky) (rel d'' y) (rel d'' l) (go foom tl d'' r)
    Nothing -> glue (rel d'' l) (rel d'' r)

bogus :: a -> Proxy# ()
bogus _ = proxy#
{-# noinline bogus #-}

replaceAlong :: (Relative k, Relative a) => BitQueue -> Delta -> a -> Map k a -> Map k a
replaceAlong !_ !_ _ Tip = error "Coda.Relative.Map.replaceAlong: failure"
replaceAlong q d x (Bin sz d' ky y l r) | !d'' <- d <> d' = case unconsQ q of
  Just (False, tl) -> Bin sz mempty (rel d'' ky) (rel d'' y) (replaceAlong tl d'' x l) (rel d'' r)
  Just (True,tl) -> Bin sz mempty (rel d'' ky) (rel d'' y) (rel d'' l) (replaceAlong tl d'' x r)
  Nothing -> Bin sz mempty (rel d'' ky) x (rel d'' l) (rel d'' r)

balanceL :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> Map k a
balanceL k x l r = case r of
  Tip -> case l of
    Tip -> Bin' 1 k x Tip Tip
    Bin _ _ _ _ Tip Tip -> Bin' 2 k x l Tip
    Bin' _ lk lx Tip (Bin' _ lrk lrx _ _) -> Bin' 3 lrk lrx (Bin' 1 lk lx Tip Tip) (Bin' 1 k x Tip Tip)
    Bin' _ lk lx ll@(Bin' _ _ _ _ _) Tip -> Bin' 3 lk lx ll (Bin' 1 k x Tip Tip)
    Bin' ls lk lx ll@(Bin' lls _ _ _ _) lr@(Bin' lrs lrk lrx lrl lrr)
      | lrs < ratio*lls -> Bin' (1+ls) lk lx ll (Bin' (1+lrs) k x lr Tip)
      | otherwise -> Bin' (1+ls) lrk lrx (Bin' (1+lls+size lrl) lk lx ll lrl) (Bin' (1+size lrr) k x lrr Tip)

  Bin' rs _ _ _ _ -> case l of
    Tip -> Bin' (1+rs) k x Tip r
    Bin' ls lk lx ll lr
      | ls > delta*rs  -> case (ll, lr) of
        (Bin' lls _ _ _ _, Bin' lrs lrk lrx lrl lrr)
          | lrs < ratio*lls -> Bin' (1+ls+rs) lk lx ll (Bin' (1+rs+lrs) k x lr r)
          | otherwise -> Bin' (1+ls+rs) lrk lrx (Bin' (1+lls+size lrl) lk lx ll lrl) (Bin' (1+rs+size lrr) k x lrr r)
        (_, _) -> error "Coda.Relative.Map.balanceL: failure"
      | otherwise -> Bin' (1+ls+rs) k x l r
{-# noinline balanceL #-}

balanceR :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
    Tip -> Bin' 1 k x Tip Tip
    Bin' _ _ _ Tip Tip -> Bin' 2 k x Tip r
    Bin' _ rk rx Tip rr@(Bin' _ _ _ _ _) -> Bin' 3 rk rx (Bin' 1 k x Tip Tip) rr
    Bin' _ rk rx (Bin' _ rlk rlx _ _) Tip -> Bin' 3 rlk rlx (Bin' 1 k x Tip Tip) (Bin' 1 rk rx Tip Tip)
    Bin' rs rk rx rl@(Bin' rls rlk rlx rll rlr) rr@(Bin' rrs _ _ _ _)
      | rls < ratio*rrs -> Bin' (1+rs) rk rx (Bin' (1+rls) k x Tip rl) rr
      | otherwise -> Bin' (1+rs) rlk rlx (Bin' (1+size rll) k x Tip rll) (Bin' (1+rrs+size rlr) rk rx rlr rr)

  Bin' ls _ _ _ _ -> case r of
    Tip -> Bin' (1+ls) k x l Tip
    Bin' rs rk rx rl rr
      | rs > delta*ls  -> case (rl, rr) of
        (Bin' rls rlk rlx rll rlr, Bin' rrs _ _ _ _)
          | rls < ratio*rrs -> Bin' (1+ls+rs) rk rx (Bin' (1+ls+rls) k x l rl) rr
          | otherwise -> Bin' (1+ls+rs) rlk rlx (Bin' (1+ls+size rll) k x l rll) (Bin' (1+rrs+size rlr) rk rx rlr rr)
        (_, _) -> error "Coda.Relative.Map.balanceR: failure"
      | otherwise -> Bin' (1+ls+rs) k x l r
{-# NOINLINE balanceR #-}

data MinView k a = MinView !k !a !(Map k a)
data MaxView k a = MaxView !k !a !(Map k a)

glue :: (Relative k, Relative a) => Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl dl kl xl ll lr) r@(Bin sr dr kr xr rl rr)
  | sl > sr = let !(MaxView km m l') = maxViewSure (rel dl kl) (rel dl xl) (rel dl ll) (rel dl lr) in balanceR km m l' r
  | otherwise = let !(MinView km m r') = minViewSure (rel dr kr) (rel dr xr) (rel dr rl) (rel dr rr) in balanceL km m l r'

minViewSure :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> MinView k a
minViewSure = go where
  go k x Tip r = MinView k x r
  go k x (Bin' _ kl xl ll lr) r = case go kl xl ll lr of
    MinView km xm l' -> MinView km xm (balanceR k x l' r)

maxViewSure :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> MaxView k a
maxViewSure = go where
  go k x l Tip = MaxView k x l
  go k x l (Bin' _ kr xr rl rr) = case go kr xr rl rr of
    MaxView km xm r' -> MaxView km xm (balanceL k x l r')

delta,ratio :: Int
delta = 3
ratio = 2

