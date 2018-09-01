{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language DeriveDataTypeable #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Located
  ( Located(..)
  ) where

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

import Algebra.Ordered
import Relative
import Delta

-- | Place a non-relative data type at a given position
--
-- Note: 'Located' is not a 'RelativeMonoid' as @'rel' 1 'mempty' '/=' 'mempty'@
data Located a = Located !Delta a
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance Hashable a => Hashable (Located a)

instance Hashable1 Located where
  liftHashWithSalt f s (Located d a) = f (hashWithSalt s d) a

instance Applicative Located where
  pure = Located mempty
  (<*>) = ap

instance Monad Located where
  Located d a >>= f = case f a of
    Located d' b -> Located (d <> d') b

instance Comonad Located where
  extract (Located _ a) = a
  extend f w@(Located d _) = Located d (f w)

instance Semigroup a => Semigroup (Located a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Located a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance MonadWriter Delta Located where
  tell d = Located d ()
  pass (Located d (a, f)) = Located (f d) a
  listen (Located d a) = Located d (a, d)

instance Relative (Located a) where
  rel d (Located d' a) = Located (d <> d') a

instance Ord a => RelativeOrder (Located a)
instance Ord a => StrictRelativeOrder (Located a)
instance OrderedMonoid a => OrderedMonoid (Located a)

instance Default a => Default (Located a) where
  def = Located mempty def

-- instance HasDelta (Located a) where delta (Located d _) = d
-- instance Ord a => HasOrderedDelta (Located a)
-- instance Monoid a => HasMonoidalDelta (Located a)
-- instance HasRelativeDelta (Located a)
