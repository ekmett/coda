module Perm where

import Numeric.Natural
import Control.Lens
import Control.Category ((>>>))
import Data.Maybe

-- a rather functional representation of permutations
data Tree = Bin !Natural !Tree !Tree | Tip
  deriving (Eq, Show)

bin :: Natural -> Natural -> Tree -> Tree -> Tree
bin i j Tip Tip | i == j = Tip
bin i j l r = Bin j l r

unit :: Natural -> Natural -> Natural -> Natural -> Tree
unit _ _ 0 i = Bin i Tip Tip
unit n s k i = case (k-1)  `divMod` 2 of
  (q,0) -> Bin n (unit n' s' q i) Tip
  (q,1) -> Bin n Tip (unit (n'+s) s' q i) 
  where n'=n+s;s'=s+s

-- TODO: avoid passing i
perm :: Functor f => Natural -> (Natural -> f Natural) -> Tree -> f Tree
perm i0 = permx 0 1 i0 i0

permx :: Functor f => Natural -> Natural -> Natural -> Natural -> (Natural -> f Natural) -> Tree -> f Tree
permx n _ _ 0 f Tip         = f n <&> \n' -> bin n n' Tip Tip
permx n _ _ 0 f (Bin j l r) = f j <&> \j' -> bin n j' l r
permx n s i k f Tip         = f i <&> \i' -> if i == i' then Tip else unit n s k i'
permx n s i k f (Bin j l r) = case (k-1) `divMod` 2 of
  (q,0) -> permx n'  s' i q f l <&> \l' -> bin n j l' r
  (q,1) -> permx n'' s' i q f r <&> \r' -> bin n j l r'
  where n'=n+s;n''=n'+s;s'=s+s

-- act :: Tree -> Natural -> Natural
-- act t i = t^.perm i

-- swap :: Natural -> Natural -> Tree
-- swap i j = Tip & perm j .~ i & perm i .~ j

instance Semigroup Tree where
 t0 <> t1 = go 0 1 t0 t1 where
   go n s Tip Tip = Tip
   go n s (Bin ai al ar) Tip           = bin n (t1^.perm ai) (gol n' s' al)    (gol n'' s' ar)   where n'=n+s;n''=n'+s;s'=s+s
   go n s Tip            (Bin _ bl br) = bin n (t1^.perm n)  (gor n' s' bl)    (gor n'' s' br)   where n'=n+s;n''=n'+s;s'=s+s
   go n s (Bin ai al ar) (Bin _ bl br) = bin n (t1^.perm ai) (go  n' s' al bl) (go n'' s' ar br) where n'=n+s;n''=n'+s;s'=s+s
   gol n s Tip = Tip
   gol n s (Bin ai al ar) = bin n (t1^.perm ai) (gol n' s' al) (gol n'' s' ar) where n'=n+s;n''=n'+s;s'=s+s
   gor n s Tip = Tip
   gor n s (Bin _ bl br) = bin n (t1^.perm n)  (gor n' s' bl) (gor n'' s' br) where n'=n+s;n''=n'+s;s'=s+s

instance Monoid Tree where
  mempty = Tip

-- work with both stored in on one tree?
data Perm = Perm Tree Tree
  deriving (Eq, Show)

inv :: Perm -> Perm
inv (Perm s t) = Perm t s

swap :: Natural -> Natural -> Perm
swap i j = Perm t t where t = Tip & perm j .~ i & perm i .~ j

act :: Perm -> Natural -> Natural
act (Perm t _) i = t^.perm i

instance Semigroup Perm where
  Perm a b <> Perm c d = Perm (a <> c) (d <> b)

instance Monoid Perm where
  mempty = Perm Tip Tip
