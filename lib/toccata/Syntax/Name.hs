{-# language GADTs #-}
-- reimplement unbound using modern ghc features, not replib?
module Syntax.Name where

import Data.Text.Prettyprint.Doc
import Data.String
import Data.Text.Short as T
import Data.Typeable

import Console.Pretty

data Name a where
  Free :: Typeable a => ShortText -> Integer -> Name a -- free
  Bound :: Typeable a => Integer -> Integer -> Name a -- bound

instance Eq (Name a) where
  Free x n  == Free x' n'  = x == x' && n == n'
  Bound x y == Bound x' y' = x == x' && y == y'
  _ == _ = False

instance Ord (Name a) where
  compare (Free x n) (Free x' n') = compare x x' <> compare n n'
  compare (Bound x y) (Bound x' y') = compare x x' <> compare y y'
  compare Free{} Bound{} = LT
  compare Bound{} Free{} = GT

instance Show (Name a) where
  showsPrec d (Free x n) = showParen (d > 10) $
    showString "Free " . showsPrec 11 x . showChar ' ' . showsPrec 11 n
  showsPrec d (Bound x y) = showParen (d > 10) $
    showString "Bound " . showsPrec 11 x . showChar ' ' . showsPrec 11 y

isBound :: Name a -> Bool
isBound Bound{} = True
isBound _ = False

isFree :: Name a -> Bool
isFree Free{} = True
isFree _ = False

instance Fancy (Name a) where
  fancy (Free x n) | T.null x = annotate StyleVar $ pretty '_' <> pretty n
  fancy (Free x 0)  = annotate StyleVar $ pretty (toText x)
  fancy (Free x n)  = annotate StyleVar $ pretty (toText x) <> pretty n
  fancy (Bound x y) = annotate StyleVar $ pretty x <> pretty '@' <> pretty y

instance Typeable a => IsString (Name a) where
  fromString n = Free (T.fromString n) 0
