{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Coda.Relative.List 
  ( List(..)
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Functor
import Control.Lens (AsEmpty(..),Cons(..), prism, uncons)
import Data.Function (on)
import Data.List (unfoldr)
import Data.Semigroup
import GHC.Exts as Exts
import Text.Read

-- | A list with an /O(1)/ 'rel', 'cons' and 'uncons', but /O(n)/ ('<>') 
data List a
  = Nil
  | Cons !Delta !a (List a)

instance Semigroup (List a) where
  Nil <> as = as
  (Cons d a as) <> bs = Cons d a (mappend as bs)

-- /O(n)/
instance Monoid (List a) where
  mempty = Nil
  mappend = (<>) 

instance (Show a, Relative a) => Show (List a) where
  showsPrec d = showsPrec d . Exts.toList

instance (Read a, Relative a) => Read (List a) where
  readPrec = Exts.fromList <$> readPrec

instance Relative a => IsList (List a) where
  type Item (List a) = a
  fromList = foldr (Cons mempty) Nil
  toList   = unfoldr uncons

instance (Eq a, Relative a) => Eq (List a) where
  (==) = (==) `on` Exts.toList
  
instance (Ord a, Relative a) => Ord (List a) where
  compare = compare `on` Exts.toList

instance Relative (List a) where
  rel _ Nil            = Nil
  rel d (Cons d' a as) = Cons (d <> d') a as

instance AsEmpty (List a) where
  _Empty = prism (const Nil) $ \case
    Nil -> Right ()
    x -> Left x

instance Relative a => Cons (List a) (List b) a b where
  _Cons = prism (uncurry (Cons mempty)) $ \case
    Nil -> Left Nil
    Cons d a as -> Right (rel d a, rel d as)

instance RelativeFunctor List where
  relmap _ Nil = Nil
  relmap f (Cons d a as) = Cons d (f a) (relmap f as)
