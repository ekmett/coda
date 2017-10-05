{-# language CPP #-}
{-# language BangPatterns #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Coda.Relative.Cat
  ( Cat
  , snocCat
  , singleton
  ) where

import Control.Lens
import Coda.Relative.Class
import Coda.Relative.Foldable
import Coda.Relative.Queue
import Data.Default
import Data.Semigroup

-- invariant, all recursive cat's are non-empty
data Cat a = E | C a (Queue (Cat a))
#if __GLASGOW_HASKELL__ >= 802
{-# complete_patterns ((:<)|C),(Empty|E) #-}
#endif

instance Relative a => Relative (Cat a) where
  rel _ E = E
  rel 0 xs = xs
  rel d (C a as) = C (rel d a) (rel d as)

instance RelativeFoldable Cat where
  rnull E = True
  rnull _ = False

  rfoldMap f !d (C a as) = f d a `mappend` rfoldMap (rfoldMap f) d as
  rfoldMap _ _ E = mempty

instance Relative a => Semigroup (Cat a) where
  xs <> E = xs
  E <> xs = xs
  C x xs <> ys = link x xs ys

instance Relative a => Monoid (Cat a) where
  mempty = E
  mappend = (<>)

link :: Relative a => a -> Queue (Cat a) -> Cat a -> Cat a
link x q ys = C x (snocQ q ys)

-- O(1 + e) where e is the # of empty nodes in the queue
linkAll :: Relative a => Queue (Cat a) -> Cat a
linkAll q = case uncons q of
  Just (cat@(C a t), q')
    | rnull q'  -> cat
    | otherwise -> link a t (linkAll q')
  Just (E, q') -> linkAll q' -- recursive case
  Nothing -> E

instance (Relative a, Relative b) => Cons (Cat a) (Cat b) a b where
  _Cons = prism kons unkons where
    kons (a, E) = C a def
    kons (a, ys) = link a def ys
    unkons E = Left E
    unkons (C a q) = Right (a, linkAll q)

singleton :: a -> Cat a
singleton a = C a def

snocCat :: Relative a => Cat a -> a -> Cat a
snocCat xs a = xs <> singleton a
