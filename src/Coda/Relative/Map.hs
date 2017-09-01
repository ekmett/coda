{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language RoleAnnotations #-}
{-# language PatternSynonyms #-}
{-# options_ghc -Wno-incomplete-patterns #-}

module Coda.Relative.Map
  ( Map
  , null
  ) where 

import Control.Lens
import Coda.Relative.Class
import Coda.Relative.Delta hiding (delta)
import Coda.Util.BitQueue
import Data.Default
import Data.Monoid
import GHC.Prim
import Prelude hiding (null)

type Size = Int

data Map k a
  = Bin {-# unpack #-} !Size {-# unpack #-} !Delta !k !a !(Map k a) !(Map k a)
  | Tip

type role Map nominal nominal

instance AsEmpty (Map k a) where
  _Empty = prism (const Tip) $ \case
    Tip -> Right ()
    x -> Left x

null :: Map k a -> Bool
null Tip = True
null _ = False

singleton :: k -> a -> Map k a
singleton k a = Bin 1 0 k a Tip Tip

size :: Map k a -> Int
size Tip = 0
size (Bin s _ _ _ _ _) = s

-- smart construction
pattern Bin_ :: (Relative k, Relative a) => Size -> k -> a -> Map k a -> Map k a -> Map k a
pattern Bin_ s k a l r <- Bin s d (rel d -> k) (rel d -> a) (rel d -> l) (rel d -> r) where
  Bin_ s k a l r = Bin s 0 k a l r

instance Relative (Map k a) where
  rel _ Tip = Tip
  rel 0 m   = m -- improve sharing
  rel d (Bin s d' k a l r) = Bin s (d <> d') k a l r

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

instance Default (Map k a) where
  def = Tip

-- instance (StrictRelativeOrder k, Relative a) => Monoid (Map k a) where
--  mempty = Tip

-- instance (StrictRelativeOrder k, Relative a) => RelativeMonoid (Map k a)

--------------------------------------------------------------------------------
-- Implementation Details
--------------------------------------------------------------------------------
 
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
    Tip -> Bin_ 1 k x Tip Tip
    Bin _ _ _ _ Tip Tip -> Bin_ 2 k x l Tip
    Bin_ _ lk lx Tip (Bin_ _ lrk lrx _ _) -> Bin_ 3 lrk lrx (Bin_ 1 lk lx Tip Tip) (Bin_ 1 k x Tip Tip)
    Bin_ _ lk lx ll@(Bin_ _ _ _ _ _) Tip -> Bin_ 3 lk lx ll (Bin_ 1 k x Tip Tip)
    Bin_ ls lk lx ll@(Bin_ lls _ _ _ _) lr@(Bin_ lrs lrk lrx lrl lrr)
      | lrs < ratio*lls -> Bin_ (1+ls) lk lx ll (Bin_ (1+lrs) k x lr Tip)
      | otherwise -> Bin_ (1+ls) lrk lrx (Bin_ (1+lls+size lrl) lk lx ll lrl) (Bin_ (1+size lrr) k x lrr Tip)

  Bin_ rs _ _ _ _ -> case l of
    Tip -> Bin_ (1+rs) k x Tip r
    Bin_ ls lk lx ll lr
      | ls > delta*rs  -> case (ll, lr) of
        (Bin_ lls _ _ _ _, Bin_ lrs lrk lrx lrl lrr)
          | lrs < ratio*lls -> Bin_ (1+ls+rs) lk lx ll (Bin_ (1+rs+lrs) k x lr r)
          | otherwise -> Bin_ (1+ls+rs) lrk lrx (Bin_ (1+lls+size lrl) lk lx ll lrl) (Bin_ (1+rs+size lrr) k x lrr r)
        (_, _) -> error "Coda.Relative.Map.balanceL: failure"
      | otherwise -> Bin_ (1+ls+rs) k x l r
{-# noinline balanceL #-}

balanceR :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
    Tip -> Bin_ 1 k x Tip Tip
    Bin_ _ _ _ Tip Tip -> Bin_ 2 k x Tip r
    Bin_ _ rk rx Tip rr@(Bin_ _ _ _ _ _) -> Bin_ 3 rk rx (Bin_ 1 k x Tip Tip) rr
    Bin_ _ rk rx (Bin_ _ rlk rlx _ _) Tip -> Bin_ 3 rlk rlx (Bin_ 1 k x Tip Tip) (Bin_ 1 rk rx Tip Tip)
    Bin_ rs rk rx rl@(Bin_ rls rlk rlx rll rlr) rr@(Bin_ rrs _ _ _ _)
      | rls < ratio*rrs -> Bin_ (1+rs) rk rx (Bin_ (1+rls) k x Tip rl) rr
      | otherwise -> Bin_ (1+rs) rlk rlx (Bin_ (1+size rll) k x Tip rll) (Bin_ (1+rrs+size rlr) rk rx rlr rr)

  Bin_ ls _ _ _ _ -> case r of
    Tip -> Bin_ (1+ls) k x l Tip
    Bin_ rs rk rx rl rr
      | rs > delta*ls  -> case (rl, rr) of
        (Bin_ rls rlk rlx rll rlr, Bin_ rrs _ _ _ _)
          | rls < ratio*rrs -> Bin_ (1+ls+rs) rk rx (Bin_ (1+ls+rls) k x l rl) rr
          | otherwise -> Bin_ (1+ls+rs) rlk rlx (Bin_ (1+ls+size rll) k x l rll) (Bin_ (1+rrs+size rlr) rk rx rlr rr)
        (_, _) -> error "Coda.Relative.Map.balanceR: failure"
      | otherwise -> Bin_ (1+ls+rs) k x l r
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
  go k x (Bin_ _ kl xl ll lr) r = case go kl xl ll lr of
    MinView km xm l' -> MinView km xm (balanceR k x l' r)

maxViewSure :: (Relative k, Relative a) => k -> a -> Map k a -> Map k a -> MaxView k a
maxViewSure = go where
  go k x l Tip = MaxView k x l
  go k x l (Bin_ _ kr xr rl rr) = case go kr xr rl rr of
    MaxView km xm r' -> MaxView km xm (balanceL k x l r')

delta,ratio :: Int
delta = 3
ratio = 2

