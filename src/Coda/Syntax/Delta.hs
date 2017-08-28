{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}

module Coda.Syntax.Delta 
  ( Delta(..)
  , Relative(..)
  ) where

import Data.Data
import Data.Hashable
import Data.Semigroup
import GHC.Generics

-- | @AlexInput@ torsor, this is a (potentially relative) position
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

instance Relative Delta where
  rel = (<>)

-- | Applying a relative position change as a left monoid action
--
-- Laws:
--
-- @
-- 'rel' 'mempty' ≡ 'id'
-- 'rel' (m '<>' n) ≡ 'rel' m . 'rel' n
-- @
class Relative t where
  rel :: Delta -> t -> t
