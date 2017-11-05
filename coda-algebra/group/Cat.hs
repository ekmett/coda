{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Relative.Cat
  ( Cat
  , snocCat
  , singleton
  , null
  ) where

import Control.Lens
import Data.Default
import Data.Function (on)
import Data.List (unfoldr)
import Data.Semigroup
import GHC.Exts as Exts
import Text.Read
import Prelude hiding (null)

import Class
import Queue hiding (null)
import qualified Queue as Q

-- invariant, all recursive cat's are non-empty
data Cat a = E | C a (Queue (Cat a))

-- {-# complete E, (:<) #-}

instance Default (Cat a) where
  def = E

instance Relative a => Relative (Cat a) where
  rel _ E = E
  rel 0 xs = xs
  rel d (C a as) = C (rel d a) (rel d as)
  {-# inline rel #-}

null :: Cat a -> Bool
null E = True
null _ = False
{-# inline null #-}

instance Relative a => Semigroup (Cat a) where
  xs <> E = xs
  E <> xs = xs
  C x xs <> ys = link x xs ys
  {-# inline (<>) #-}

instance Relative a => Monoid (Cat a) where
  mempty = E
  mappend = (<>)

link :: Relative a => a -> Queue (Cat a) -> Cat a -> Cat a
link x q ys = C x (snocQ q ys)
{-# inline link #-}

-- O(1 + e) where e is the # of empty nodes in the queue
linkAll :: Relative a => Queue (Cat a) -> Cat a
linkAll q = case uncons q of
  Just (cat@(C a t), q')
    | Q.null q' -> cat
    | otherwise -> link a t (linkAll q')
  Just (E, q') -> linkAll q' -- recursive case
  Nothing -> E

instance AsEmpty (Cat a) where
  _Empty = prism (const E) $ \case
    E -> Right ()
    xs -> Left xs

instance (Relative a, Relative b) => Cons (Cat a) (Cat b) a b where
  _Cons = prism kons unkons where
    kons (a, E) = C a def
    kons (a, ys) = link a def ys
    {-# inline conlike kons #-}
    unkons E = Left E
    unkons (C a q) = Right (a, linkAll q)
    {-# inline unkons #-}

instance Relative a => IsList (Cat a) where
  type Item (Cat a) = a
  fromList = foldr cons E
  {-# inline fromList #-}
  toList = unfoldr uncons
  {-# inline toList #-}

singleton :: a -> Cat a
singleton a = C a def
{-# inline conlike singleton #-}

snocCat :: Relative a => Cat a -> a -> Cat a
snocCat xs a = xs <> singleton a
{-# inline snocCat #-}

instance (Show a, Relative a) => Show (Cat a) where
  showsPrec d = showsPrec d . Exts.toList

instance (Read a, Relative a) => Read (Cat a) where
  readPrec = Exts.fromList <$> readPrec

instance (Eq a, Relative a) => Eq (Cat a) where
  (==) = (==) `on` Exts.toList
  {-# inline (==) #-}

instance (Ord a, Relative a) => Ord (Cat a) where
  compare = compare `on` Exts.toList
  {-# inline compare #-}

