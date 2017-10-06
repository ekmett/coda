{-# language TypeFamilies #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language FlexibleInstances #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}
{-# language UndecidableInstances #-}
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
-- File lines
---------------------------------------------------------------------------------

module Coda.Syntax.Line
  (
  -- * Lines
    Line(..)
  , foldLines
  , FromText(..)
  -- * Summarizing Lines
  , LineMeasure(..)
  , HasLineMeasure(..)
  ) where

import Coda.Relative.Delta
import Control.Comonad
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

class FromText a where
  fromText :: Text -> a

-- | Invariants
--
-- * The only occurrences of '\n', '\r' or "\r\n" are at the end of the Text
data Line a = Line { runLine :: !Text, content :: a }
  deriving (Eq, Ord, Show, Read, Hashable, Generic, Functor, Foldable, Traversable, Generic1)

instance Comonad Line where
  extract (Line _ a) = a
  duplicate w@(Line t _) = Line t w

instance FromText a => FromText (Line a) where
  fromText t = Line t (fromText t)

instance FromText a => IsString (Line a) where
  fromString = fromText . fromString

instance HasDelta (Line a) where
  delta = Delta #. Text.lengthWord16 . runLine

instance FromText a => Default (Line a) where
  def = fromText ""

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
data LineMeasure v = LineMeasure !Int !Delta v
  deriving (Eq, Ord, Show, Read, Data, Generic, Generic1, Functor, Foldable, Traversable)

instance Comonad LineMeasure where
  extract (LineMeasure _ _ v) = v
  duplicate w@(LineMeasure l d _) = LineMeasure l d w

instance HasDelta (LineMeasure v) where
  delta (LineMeasure _ d _) = d

instance Hashable v => Hashable (LineMeasure v)

instance Semigroup v => Semigroup (LineMeasure v) where
  LineMeasure l d v <> LineMeasure l' d' v' = LineMeasure (l + l') (d <> d') (v <> v')

instance Monoid v => Monoid (LineMeasure v) where
  mempty = LineMeasure 0 0 mempty
  mappend (LineMeasure l d v) (LineMeasure l' d' v') = LineMeasure (l + l') (d <> d') (mappend v v')

instance Measured v a => Measured (LineMeasure v) (Line a) where
  measure (Line l a) = LineMeasure 1 (Delta $ Text.lengthWord16 l) (measure a)

instance Default v => Default (LineMeasure v) where
  def = LineMeasure 0 0 def

--------------------------------------------------------------------------------
-- HasLineMeasure
--------------------------------------------------------------------------------

class HasDelta t => HasLineMeasure t v | t -> v where
  lineMeasure :: t -> LineMeasure v

  lineCount :: t -> Int
  lineCount = lineCount . lineMeasure

instance HasLineMeasure (LineMeasure v) v where
  lineCount (LineMeasure l _ _) = l
  lineMeasure = id

instance Measured v a => HasLineMeasure (Line a) v where
  lineMeasure = measure

