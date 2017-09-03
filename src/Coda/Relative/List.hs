{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RoleAnnotations #-}
{-# language PatternSynonyms #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Relative.List
  ( List(..)
  , pattern Cons'
  , reverse
  , rtoRList
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Foldable
import Control.Lens (AsEmpty(..),prism, uncons, Cons(..))
import Data.Default
import Data.Function (on)
import Data.List (unfoldr)
import Data.Semigroup
import GHC.Exts as Exts
import Prelude hiding (reverse)
import Text.Read

-- | A list with an /O(1)/ 'rel', 'cons' and 'uncons', but /O(n)/ ('<>')
data List a
  = Nil
  | Cons !Delta !a (List a)

type role List nominal

rtoRList :: RelativeFoldable f => f a -> List a
rtoRList = rfoldr Cons Nil 0

pattern Cons' :: Relative a => () => a -> List a -> List a
pattern Cons' a as <- Cons d (rel d -> a) (rel d -> as) where
  Cons' a as = Cons mempty a as

reverse :: Relative a => List a -> List a
reverse = go mempty Nil where
  go !d !acc (Cons d' a as) | d'' <- mappend d d' = go d'' (Cons mempty (rel d'' a) acc) as
  go _   acc Nil = acc

instance RelativeFoldable List where
  rfoldMap f !d (Cons d' a as) | !d'' <- mappend d d' = f d'' a `mappend` rfoldMap f d'' as
  rfoldMap _ _ Nil = mempty

  rfoldr f z !d (Cons d' a as) | !d'' <- d <> d' = f d'' a (rfoldr f z d'' as)
  rfoldr _ z _ Nil = z

  rnull Nil = True
  rnull _ = False

  rlength = go 0 where
    go !n Nil = n
    go n (Cons _ _ as) = go (n+1) as

instance RelativeFoldableWithIndex Int List where
  irfoldMap = go 0 where
    go !_ !_ !_ Nil = mempty
    go i f d (Cons d' a as) | !d'' <- mappend d d' = f d'' i a `mappend` go (i+1) f d'' as

instance (Show a, Relative a) => Show (List a) where
  showsPrec d = showsPrec d . Exts.toList

instance (Read a, Relative a) => Read (List a) where
  readPrec = Exts.fromList <$> readPrec

instance (Eq a, Relative a) => Eq (List a) where
  (==) = (==) `on` Exts.toList

instance (Ord a, Relative a) => Ord (List a) where
  compare = compare `on` Exts.toList

instance RelativeOrder a => RelativeOrder (List a)
instance StrictRelativeOrder a => StrictRelativeOrder (List a)
instance RelativeMonoid (List a)

-- /O(n)/
instance Semigroup (List a) where
  Nil <> as = as
  Cons d a as <> bs = Cons d a (mappend as bs)

-- /O(n)/
instance Monoid (List a) where
  mempty = Nil
  mappend Nil xs = xs
  mappend (Cons d a as) bs = Cons d a (mappend as bs)

instance Relative a => IsList (List a) where
  type Item (List a) = a
  fromList = foldr Cons' Nil
  toList   = unfoldr uncons

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

instance Default (List a) where
  def = Nil
