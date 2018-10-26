{-# language BangPatterns #-}
module Perm 
( Perm
, act
, swap
, fresh
, inv
, greater
, sup
, rcycles
, cycles
, cyclic
, reassemble
) where

import Numeric.Natural
import Control.Lens
import Control.Category ((>>>))
import Data.List (groupBy)
import Data.Maybe
import Data.Semigroup

-- the int is the depth of the shallowest free variable

-- a rather functional representation of finitely generated permutations of the naturals as trees
-- for use by nominal sets
data Tree = Tip | Bin !Natural !Int !Natural !Tree !Tree
  deriving (Show)

instance Eq Tree where
  Tip == Tip = True
  Bin _ _ na la ra == Bin _ _ nb lb rb = na == nb && la == lb && ra == rb
  _ == _ = False

-- this puts trees in _some_ canonical order. good luck describing it
instance Ord Tree where
  Tip `compare` Tip = EQ
  Bin _ _ na la ra `compare` Bin _ _ nb lb rb = compare na nb <> compare la lb <> compare ra rb
  Tip `compare` Bin{} = LT
  Bin{} `compare` Tip = GT

supTree :: Tree -> Maybe (Max Natural)
supTree Tip = Nothing
supTree (Bin s _ _ _ _) = Just (Max s)

dep :: Tree -> Int
dep Tip = 0
dep (Bin _ i _ _ _) = i

bin :: Natural -> Natural -> Tree -> Tree -> Tree
bin i j l r = case supTree l <> supTree r of
  Nothing | i == j -> Tip -- we don't exist
          | otherwise -> Bin i 0 j l r -- we're the biggest seen so far, l and r are tips
  Just (Max m) -> Bin m (if i == j then 0 else min (dep l) (dep r) + 1) j l r

unit :: Natural -> Natural -> Natural -> Natural -> Natural -> Tree
unit t _ _ 0 i = Bin t 1 i Tip Tip -- left and right are both free, we're the largest seen so locally
unit t n s k i = case (k-1)  `divMod` 2 of
  (q,0) -> Bin t 0 n (unit t n' s' q i) Tip     -- n is free
  (q,1) -> Bin t 0 n Tip (unit t (n'+s) s' q i) -- n is free
  where n'=n+s;s'=s+s

-- TODO: avoid passing i
perm :: Functor f => Natural -> (Natural -> f Natural) -> Tree -> f Tree
perm i0 = permx 0 1 i0 i0

permx :: Functor f => Natural -> Natural -> Natural -> Natural -> (Natural -> f Natural) -> Tree -> f Tree
permx n _ _ 0 f Tip             = f n <&> \n' -> bin n n' Tip Tip
permx n _ _ 0 f (Bin _ _ j l r) = f j <&> \j' -> bin n j' l r
permx n s i k f Tip             = f i <&> \i' -> if i == i' then Tip else unit i n s k i'
permx n s i k f (Bin _ _ j l r) = case (k-1) `divMod` 2 of
  (q,0) -> permx n'  s' i q f l <&> \l' -> bin n j l' r
  (q,1) -> permx n'' s' i q f r <&> \r' -> bin n j l r'
  where n'=n+s;n''=n'+s;s'=s+s

instance Semigroup Tree where
 t0 <> t1 = go 0 1 t0 t1 where
   go n s Tip                Tip               = Tip
   go n s (Bin _ _ ai al ar) Tip               = bin n (t1^.perm ai) (gol n' s' al)    (gol n'' s' ar)   where n'=n+s;n''=n'+s;s'=s+s
   go n s Tip                (Bin _ _ _ bl br) = bin n (t1^.perm n)  (gor n' s' bl)    (gor n'' s' br)   where n'=n+s;n''=n'+s;s'=s+s
   go n s (Bin _ _ ai al ar) (Bin _ _ _ bl br) = bin n (t1^.perm ai) (go  n' s' al bl) (go n'' s' ar br) where n'=n+s;n''=n'+s;s'=s+s
   gol n s Tip = Tip
   gol n s (Bin _ _ ai al ar) = bin n (t1^.perm ai) (gol n' s' al) (gol n'' s' ar) where n'=n+s;n''=n'+s;s'=s+s
   gor n s Tip = Tip
   gor n s (Bin _ _ _ bl br) = bin n (t1^.perm n)  (gor n' s' bl) (gor n'' s' br) where n'=n+s;n''=n'+s;s'=s+s

instance Monoid Tree where
  mempty = Tip

-- storing both lets us invert a permutation in O(1)
data Perm = Perm Tree Tree
  deriving Show

instance Eq Perm where
  Perm x _ == Perm y _ = x == y

instance Ord Perm where
  Perm x _ `compare` Perm y _ = compare x y

inv :: Perm -> Perm
inv (Perm s t) = Perm t s

swap :: Natural -> Natural -> Perm
swap i j = Perm t t where t = Tip & perm j .~ i & perm i .~ j

class Act s where
  act :: Perm -> s -> s

instance Act Natural where 
  act (Perm t _) i = t^.perm i

-- conjugation
instance Act Perm where
  act s t = inv s <> t <> s

instance Semigroup Perm where
  Perm a b <> Perm c d = Perm (a <> c) (d <> b)

instance Monoid Perm where
  mempty = Perm Tip Tip

data Stream = !Natural :- Stream
  deriving Show

-- finds an infinite sequence of variables that do not participate in the permutation, placed close to the root
-- the first entry is the 'shallowest' variable id. after that we continue down the tree found until we can produce
-- an infinite family of free variables by using a stride found by the shape of the tree as a 'ray' of variables
-- with some step
fresh :: Perm -> Stream
fresh (Perm t _) = freshTree 0 1 t where
  freshTree n s Tip = fill n s where fill !n !s = n :- fill (n + s) s
  freshTree n s (Bin _ d _ l r) 
    | dl <= dr = tweak (freshTree n' s' l) 
    | otherwise = tweak (freshTree n'' s' r)
    where dl=dep l;dr=dep r;n'=n+s;n''=n'+s;s'=s+s;
           -- grab opportunistic fresh variables near the root on the way down
          tweak | d == 0 = (n :-)
                | otherwise = id

-- modulus of continuity
sup :: Perm -> Maybe Natural
sup (Perm t _) = getMax <$> supTree t

-- all n >= greater xs do not participate. This is the start of the smallest ray of step size 1
greater :: Perm -> Natural
greater (Perm t _) = maybe 0 (succ . getMax) (supTree t)

-- this is not quite natural order, as its easiest for me to find the largest element and work backwards. for natural order, reverse the list
-- cycles
rcycles :: Perm -> [[Natural]]
rcycles (Perm t0 _) = go t0 where
  go t = case supTree t of
    Nothing -> []
    Just (Max m) -> case peel m m t of
      (t',xs) -> xs : go t'

-- mangles the tree to remove this cycle as we go
peel :: Natural -> Natural -> Tree -> (Tree, [Natural])
peel m e t = case t & perm e <<.~ e of
  (n, t') | n == m    -> (t', [e])
          | otherwise -> (e :) <$> peel m n t'

-- standard cyclic representation of a permutation, broken into parts
cycles :: Perm -> [[Natural]]
cycles = reverse . rcycles

-- standard cyclic representation of a permutation, smashed flat
cyclic :: Perm -> [Natural]
cyclic = concat . cycles

-- reassemble takes a standard cyclic representation smashed flat and reassembles the cycles
reassemble :: [Natural] -> [[Natural]] 
reassemble = groupBy (>)

{-
-- Metric ~ Maybe (Min (Natural, Bool))
data Metric = MLT !Natural | MEQ | MGT !Natural

instance Semigroup Metric where
  x <> MEQ = x
  MEQ <> x = x
  MLT a <> MLT b = MLT (min a b)
  MGT a <> MGT b = MGT (min a b)
  m@(MLT a) <> n@(MGT b)
    | a <= b    = m
    | otherwise = n
  m@(MGT a) <> n@(MLT b)
    | a < b     = m
    | otherwise = n
    
mcompare :: Natural -> Natural -> Natural -> Tree -> Tree -> Metric
mcompare _ n s Tip Tip = MEQ
mcompare _ n _ Tip Bin{} = MLT n -- this is not _quite_ correct
mcompare _ n _ Bin{} Tip = MGT n 
mcompare c n s (Bin _ _ ia la ra) (Bin _ _ ib lb rb) 
  | n > c = MEQ -- cutoff
  | otherwise = case compare ia ib of
    LT -> MLT n
    GT -> MGT n
    EQ -> case mcompare c n' s' la lb of
      MEQ        -> mcompare c n'' s' ra rb
      m@(MLT c') -> m <> mcompare c' n'' s' ra rb -- use hard cutoff
      m@(MGT c') -> m <> mcompare c' n'' s' ra rb -- use hard cutoff
  where n'=n+s;n''=n'+s;s'=s+s
  
instance Ord Tree where
  compare t0 t1 = mcompare 0 1 (maybe 0 (succ . getMax) (supTree t0 <> supTree t1) t0 t1
-}

