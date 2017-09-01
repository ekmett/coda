{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
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

module Coda.Syntax.Line
  (
  -- * Lines
    Line(..)
  , HasLine(..)
  -- * Summarizing Lines
  , LineMeasure(..)
  , HasLineMeasure(..)
  -- * Position <-> Delta
  , positionToDelta
  , deltaToPosition
  ) where

import Language.Server.Protocol (Position(..))
import Coda.Relative.Delta
import Data.Data
import Data.Default
import Data.FingerTree
import Data.Hashable
import Data.Semigroup
import Data.String
import Data.Text (Text)
import qualified Data.Text.Unsafe as Text
import GHC.Generics

-- | Invariants
--
-- * The only occurrences of '\n', '\r' or "\r\n" are at the end of the Text
newtype Line = Line Text
  deriving (Eq,Ord,Show,Read,Hashable,Generic)

instance IsString Line where
  fromString = Line . fromString

instance HasDelta Line where
  delta (Line l) = Text.lengthWord16 l

instance Default Line where
  def = Line ""

--------------------------------------------------------------------------------
-- HasLine
--------------------------------------------------------------------------------

class HasLine t where
  line :: t -> Line

instance HasLine Line where
  line = id

--------------------------------------------------------------------------------
-- LineMeasure
--------------------------------------------------------------------------------

-- | 'LineMeasure' is not 'Position'
--
-- This measures line count and total number of code-units over all lines
-- not the number of code-units on the current line.
--
-- This makes the result a proper Abelian group, not just a Monoid.
--
-- Also when used to measure a 'FingerTree' you can use the 'FingerTree' to convert
-- back and forth between 'Delta' and 'Position' in /O(log l)/ time, while still
-- letting us use the compact single-integer Abelian group 'Delta' representation
-- internally for almost all positioning.
data LineMeasure = LineMeasure !Int !Int
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance HasDelta LineMeasure where
  delta (LineMeasure _ d) = d

instance Hashable LineMeasure

instance Semigroup LineMeasure where
  LineMeasure l d <> LineMeasure l' d' = LineMeasure (l + l') (d + d')

instance Monoid LineMeasure where
  mempty = LineMeasure 0 0
  mappend = (<>)

instance Measured LineMeasure LineMeasure where
  measure = id

instance Default LineMeasure where
  def = mempty

--------------------------------------------------------------------------------
-- HasLineMeasure
--------------------------------------------------------------------------------

class HasDelta t => HasLineMeasure t where
  lineCount :: t -> Int

instance HasLineMeasure LineMeasure where
  lineCount (LineMeasure l _) = l

--------------------------------------------------------------------------------
-- Position <-> Delta
--------------------------------------------------------------------------------

-- | Compute an Language Server Protocol 'Position' from a 'Delta' given the associated text
--
-- Takes /O(log l)/ where l is the number of lines
deltaToPosition :: (Measured v a, HasLineMeasure v) => FingerTree v a -> Delta -> Position
deltaToPosition t (Delta d) = case split (\x -> delta x >= d) t of
  (l, _) | ml <- measure l -> Position (lineCount ml) (d - delta ml)

-- | Convert from a Language Server Protocol 'Position' to a 'Delta' given the associated text.
--
-- /O(log l)/ where l is the number of lines
positionToDelta :: (Measured v a, HasLineMeasure v) => FingerTree v a -> Position -> Delta
positionToDelta t (Position nl c16) = case split (\x -> lineCount x >= nl) t of
  (l, _) -> Delta $ delta (measure l) + c16
