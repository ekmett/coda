{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}

module Coda.Syntax.Prefix where

import Coda.Syntax.Rope
import Data.Char (isSpace)
import Data.Data hiding (Prefix)
import Data.Semigroup
import Data.String
import Data.Text as Text
import GHC.Generics hiding (Prefix)
import Prelude

-- | zero <> a = zero = a <> zero
class Semigroup a => SemigroupWithZero a where zero :: a
instance Num a => SemigroupWithZero (Product a) where zero = Product 0
instance (Ord a, Bounded a) => SemigroupWithZero (Max a) where zero = Max minBound
instance (Ord a, Bounded a) => SemigroupWithZero (Min a) where zero = Min maxBound

-- | line prefixes form a semigroup with a zero 
newtype Prefix = Prefix Text deriving (Eq,Show,Generic,Data)

joinAndCompare :: Prefix -> Prefix -> Either Prefix Ordering
joinAndCompare (Prefix xs) (Prefix ys) = case commonPrefixes xs ys of
    Just (c, l, r) -> cmp c l r
    Nothing        -> cmp "" xs ys
  where
    cmp _ "" "" = Right EQ
    cmp _ "" _  = Right LT
    cmp _ _  "" = Right GT
    cmp c _  _  = Left (Prefix c)

instance Semigroup Prefix where
  Prefix xs <> Prefix ys
    = Prefix $ case commonPrefixes xs ys of
      Just (zs, _, _) -> zs
      Nothing         -> ""

instance SemigroupWithZero Prefix where
  zero = Prefix ""

instance FromText Prefix where
  fromText = Prefix . Text.takeWhile isSpace

instance IsString Prefix where
  fromString = Prefix . pack . Prelude.takeWhile isSpace
