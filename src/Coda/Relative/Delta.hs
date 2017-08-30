{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}

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
  ) where

import Data.Data
import Data.Hashable
import Data.Semigroup
import GHC.Generics

-- | This is a (potentially relative) position
data Delta = Delta
  { deltaLine  :: !Int -- 0-based line offset
  , deltaCol16 :: !Int -- utf-16 code unit offset into the current line
  , deltaCol8  :: !Int -- utf-8 byte offset into the current line
  } deriving (Eq, Ord, Show, Read, Data, Generic, Hashable)

instance Semigroup Delta where
  Delta a b c <> Delta 0 e f = Delta a (b + e) (c + f)
  Delta a _ _ <> Delta d e f = Delta (a + d) e f

instance Monoid Delta where
  mempty = Delta 0 0 0
  mappend = (<>)
