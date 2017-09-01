{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language DeriveDataTypeable #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Relative.At
  ( At(..)
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Control.Applicative
import Control.Comonad
import Control.Monad (ap)
import Control.Monad.Writer.Class
import Data.Data
import Data.Default
import Data.Hashable
import Data.Hashable.Lifted
import Data.Semigroup
import GHC.Generics

-- | Place a non-relative data type at a given position
--
-- Note: 'At' is not a 'RelativeMonoid' as @'rel' 1 'mempty' '/=' 'mempty'@
data At a = At !Delta a
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance Hashable a => Hashable (At a)

instance Hashable1 At where
  liftHashWithSalt f s (At d a) = f (hashWithSalt s d) a

instance Applicative At where
  pure = At mempty
  (<*>) = ap

instance Monad At where
  At d a >>= f = case f a of
    At d' b -> At (d <> d') b

instance Comonad At where
  extract (At _ a) = a
  extend f w@(At d _) = At d (f w)

instance HasDelta (At a) where
  delta (At d _) = d

instance Monoid a => Monoid (At a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance MonadWriter Delta At where
  tell d = At d ()
  pass (At d (a, f)) = At (f d) a
  listen (At d a) = At d (a, d)

instance Relative (At a) where
  rel d (At d' a) = At (d <> d') a

instance Ord a => HasOrderedDelta (At a)
instance Monoid a => HasMonoidalDelta (At a)
instance HasRelativeDelta (At a)
instance Ord a => RelativeOrder (At a)
instance Ord a => StrictRelativeOrder (At a)
instance OrderedMonoid a => OrderedMonoid (At a)

instance Default a => Default (At a) where
  def = At def def
