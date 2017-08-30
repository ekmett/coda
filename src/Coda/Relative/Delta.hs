{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
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

module Coda.Relative.Delta 
  ( Delta(..)
  , HasDelta(..)
  ) where

import Data.Data
import Data.FingerTree
import Data.Hashable
import Data.Semigroup
import GHC.Generics

newtype Delta = Delta Int 
  deriving (Eq,Ord,Show,Read,Data,Generic,Num)

instance Hashable Delta

instance Measured Delta Delta where
  measure = id

instance Semigroup Delta where
  (<>) = (+)

instance Monoid Delta where
  mempty = 0
  mappend = (+)

class HasDelta t where
  delta :: t -> Int

instance HasDelta Delta where
  delta (Delta a) = a
