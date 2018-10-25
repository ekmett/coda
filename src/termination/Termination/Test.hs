{-# language CPP #-}
{-# language ViewPatterns #-}
{-# language GADTs #-}
{-# language ParallelListComp #-}

-- |
--
-- Well-Quasi-Orders
--
-- Based on <https://pdfs.semanticscholar.org/104e/66d8f7c12de880923b036739d7219de31034.pdf Termination Combinators Forever>
-- by Bolingbroke et al.

module Termination.Test where

import Termination.History
import Termination.Pair
import Control.Arrow
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import Data.Void

-- | A well quasi-order: A reflexive, transitive relation such that
-- and every infinite set xs has i < j such that (xs!i) <= (xs!j)
-- returns both x <= y and y <= x simultaneously, and uses a function
-- for more efficient prep
data Test a where
  Test :: (a -> s) -> (s -> s -> BB) -> Test a

runTest :: Test a -> a -> a -> Bool
runTest (Test p f) a b = bb1 $ f (p a) (p b)

instance Contravariant Test where
  contramap f (Test g k) = Test (g . f) k

instance Divisible Test where
  conquer = Test id mempty
  divide f (Test p g) (Test q h) = Test pq gh where
    pq (f -> (l, r)) = (l', r') where !l' = p l; !r' = q r
    gh (a, b) (c, d) = g a c <> h b d

instance Decidable Test where
  lose f = Test f absurd
  choose f (Test p g) (Test q h) = Test (fmap (p+++q) f) step where
    step (Left a) (Left b) = g a b
    step (Right a) (Right b) = h a b
    step _ _ = FF

instance Semigroup (Test a) where
  Test p g <> Test q h = Test (p &&& q) $ \(a,b) (c,d) -> g a c <> h b d

instance Monoid (Test a) where
  mempty = conquer
  mappend = (<>)

-- set :: Ord a => Test a -> Test [a]
-- set (Test f p) = Test (map f) $ \xs ys -> go xs ys `bb` go ys xs where
--  go xs ys = all (\x -> any (\y -> bb1 $ p x y) ys) xs

-- side-condition: needs 'a' to be finitely enumerable
finite :: Eq a => Test a
finite = Test id $ \x y -> diagBB $ x == y -- symmetric

-- side-condition: well-founded
ord :: Ord a => Test a
ord = Test id $ \x y -> ordBB $ compare x y

-- @partial f@ requires f is a well-partial-order
partial :: (a -> a -> Maybe Ordering) -> Test a
partial f = Test id $ \x y -> pordBB $ f x y

history :: Test a -> History a
history (Test p f) = History step [] where
  step xs x
    | any bb1 ys = Nothing
    | otherwise = Just $ z : [ x' | x' <- xs | False <- bb2 <$> ys ] where
      z  = p x
      ys = f z <$> xs
