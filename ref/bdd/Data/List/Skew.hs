{-# language BangPatterns #-}
{-# language DeriveTraversable #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Data.List.Skew 
  ( Skew(Cons, Nil)
  , uncons
  , index
  ) where

import qualified Data.List as List
import GHC.Exts as Exts

instance Exts.IsList (Skew a) where
  type Item (Skew a) = a
  fromList = foldr cons Nil
  toList = List.unfoldr uncons

-- | Skew binary random access list a la Okasaki
data Skew a = Skew !Int !(Tree a) !(Skew a) | Nil
  deriving (Eq,Ord,Show,Functor,Traversable)

{-# complete Cons, Nil #-}
{-# complete Skew, Nil #-}

-- /O(1)/
pattern Cons :: a -> Skew a -> Skew a
pattern Cons x xs <- (uncons -> Just (x, xs)) where
  Cons x xs = cons x xs

instance Foldable Skew where
  length (Skew n _ xs) = n + length xs
  length Nil = 0
  foldMap _ Nil = mempty
  foldMap f (Skew _ xs ys) = foldMap f xs `mappend` foldMap f ys
  null Nil = True
  null _ = False

data Tree a = Bin !Int a !(Tree a) !(Tree a) | Tip a
  deriving (Eq,Ord,Show,Functor, Traversable)

instance Foldable Tree where
  length (Bin n _ _ _) = n
  length (Tip _) = 1
  null _ = False
  foldMap f (Tip a) = f a
  foldMap f (Bin _ a l r) = f a `mappend` foldMap f l `mappend` foldMap f r

cons :: a -> Skew a -> Skew a
cons a (Skew n xs (Skew _ ys zs)) | n == n = Skew (2*n+1) (Bin n a xs ys) zs
cons a ys = Skew 1 (Tip a) ys

-- /O(1)/
uncons :: Skew a -> Maybe (a, Skew a)
uncons (Skew _ (Bin s a l r) ys) = Just (a, Skew s l (Skew s r ys))
uncons (Skew _ (Tip a) ys) = Just (a, ys)
uncons Nil = Nothing

-- /O(log n)/
index :: Skew a -> Int -> a
index (Skew n xs ys) !i0
  | i0 >= n = index ys (i0 - n)
  | otherwise = go i0 xs
  where
     go _ (Tip a) = a
     go i (Bin s a l r)
       | i == 0    = a
       | i <= s    = go (i-1) l 
       | otherwise = go (i-1-s) r
index Nil _ = error "Skew.index: out of bounds"
