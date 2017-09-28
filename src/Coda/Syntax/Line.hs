{-# language TypeFamilies #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- File lines
---------------------------------------------------------------------------------

module Coda.Syntax.Line
  (
  -- * Lines
    Line(..)
  , HasLine(..)
  , foldLines
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
import Data.Profunctor.Unsafe
import Data.Semigroup
import Data.String
import Data.Text.Internal (Text(..))
import qualified Data.Text.Unsafe as Text
import qualified Data.Text.Array as Text
import GHC.Generics

-- | Invariants
--
-- * The only occurrences of '\n', '\r' or "\r\n" are at the end of the Text
newtype Line = Line { runLine :: Text }
  deriving (Eq,Ord,Show,Read,Hashable,Generic)

instance IsString Line where
  fromString = Line . fromString

instance HasDelta Line where
  delta = Delta #. Text.lengthWord16 .# runLine

instance Default Line where
  def = Line ""

cr, lf, crlf :: Text
cr   = "\r"
lf   = "\n"
crlf = "\r\n"

-- TODO: This should split up in parallel with a monoid
-- TODO: Consider copying the Text to new arrays. That way when
-- we do incremental updates on the Text we don't need to hold the
-- entire original Text. OTOH, this would expand our memory footprint
-- and the common case is that we do small edits to big initial
-- sets of lines
foldLines :: (a -> Text -> a) -> a -> Text -> a
foldLines f z0 (Text a0 o0 l0) = go o0 o0 (o0+l0) a0 z0 where
  -- go :: Int -> Int -> Int -> Array -> a -> a
  go !s !i !e !a z
    | i < e = case Text.unsafeIndex a i of
      10             -> go (i+1) (i+1) e a $ f z $ if s < i then Text a s (i+1-s) else lf
      13 | i+1 < e -> case Text.unsafeIndex a (i+1) of
           10        -> go (i+2) (i+2) e a $ f z $ if s < i then Text a s (i+2-s) else crlf
           _         -> go (i+1) (i+1) e a $ f z $ if s < i then Text a s (i+1-s) else cr
         | otherwise -> go (i+1) (i+1) e a $ f z $ if s < i then Text a s (i+1-s) else cr
      _ -> go s (i+1) e a z
    | s < e = f z $ Text a s (e-s)
    | otherwise = z

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
data LineMeasure = LineMeasure !Int !Delta
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance HasDelta LineMeasure where
  delta (LineMeasure _ d) = d

instance Hashable LineMeasure

instance Semigroup LineMeasure where
  LineMeasure l d <> LineMeasure l' d' = LineMeasure (l + l') (d <> d')

instance Monoid LineMeasure where
  mempty = LineMeasure 0 0
  mappend = (<>)

instance Measured LineMeasure LineMeasure where
  measure = id

instance Measured LineMeasure Line where
  measure (Line l) = LineMeasure 1 $ Delta $ Text.lengthWord16 l

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
deltaToPosition t (Delta d) = case split (\x -> units x >= d) t of
  (l, _) | ml <- measure l -> Position (lineCount ml) (d - units ml)

-- | Convert from a Language Server Protocol 'Position' to a 'Delta' given the associated text.
--
-- /O(log l)/ where l is the number of lines
positionToDelta :: (Measured v a, HasLineMeasure v) => FingerTree v a -> Position -> Delta
positionToDelta t (Position nl c16) = case split (\x -> lineCount x >= nl) t of
  (l, _) -> Delta $ units (measure l) + c16
