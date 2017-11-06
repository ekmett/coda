{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language DeriveDataTypeable #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Relative.Absolute
  ( Absolute(..)
  ) where

import Control.Lens
import Coda.Relative.Class
import Data.Data
import Data.Hashable
import GHC.Generics

-- | Make anything "Relative" trivially.
--
-- Requests for its 'delta' are passed through unmodified.
newtype Absolute a = Absolute a
  deriving (Eq,Ord,Show,Read,Data,Generic)

makeWrapped ''Absolute

instance Relative (Absolute a) where rel _ x = x
instance Ord a => RelativeOrder (Absolute a)
instance Monoid a => Monoid (Absolute a) where
  mempty = Absolute mempty
  mappend (Absolute a) (Absolute b) = Absolute (mappend a b)

instance Monoid a => RelativeMonoid (Absolute a)
instance Ord a => StrictRelativeOrder (Absolute a)
instance Hashable a => Hashable (Absolute a)

-- instance HasDelta a => HasDelta (Absolute a) where delta (Absolute a) = delta a
-- instance HasMonoidalDelta a => HasMonoidalDelta (Absolute a)
-- instance HasOrderedDelta a => HasOrderedDelta (Absolute a)
