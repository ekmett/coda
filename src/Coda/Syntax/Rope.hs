{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language RoleAnnotations #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Syntax.Rope
  ( Rope(..)
  , splitAtPosition
  , splitAtDelta
  , insertAt
  , deleteRange
  , replaceRange
  , deltaToPosition
  , positionToDelta
  -- * Lines
  , Line(..)
  , foldLines
  , FromText(..)
  -- * Summarizing Lines
  , LineMeasure(..)
  , HasLineMeasure(..)
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Control.Comonad
import Data.Data
import Data.Default
import qualified Data.FingerTree as F
import Data.FingerTree hiding (SearchResult(..))
import Data.Hashable
import Data.Profunctor.Unsafe
import Data.Semigroup
import Data.String
import Data.Text as Text hiding (split)
import qualified Data.Text.Array as Text
import Data.Text.Internal (Text(..))
import Data.Text.Unsafe as Text
import GHC.Generics
import Language.Server.Protocol hiding (error)

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
--
data LineMeasure v = LineMeasure !Int !Delta v
  deriving (Eq, Ord, Show, Read, Data, Generic, Generic1, Functor, Foldable, Traversable)

instance Comonad LineMeasure where
  extract (LineMeasure _ _ v) = v
  duplicate w@(LineMeasure l d _) = LineMeasure l d w

instance HasDelta (LineMeasure v) where
  delta (LineMeasure _ d _) = d

instance Hashable v => Hashable (LineMeasure v)

instance RelativeMonoid v => Semigroup (LineMeasure v) where
  LineMeasure l d v <> LineMeasure l' d' v' = LineMeasure (l + l') (d + d') (mappend v (rel d v'))

instance RelativeMonoid v => Monoid (LineMeasure v) where
  mempty = LineMeasure 0 0 mempty
  mappend (LineMeasure l d v) (LineMeasure l' d' v') = LineMeasure (l + l') (d + d') (mappend v (rel d v'))

-- Measured v a is somewhat the wrong constraint here
instance (RelativeMonoid v, Measured v a) => Measured (LineMeasure v) (Line a) where
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

instance (RelativeMonoid v, Measured v a) => HasLineMeasure (Line a) v where
  lineMeasure = measure

--------------------------------------------------------------------------------
-- Ropes
--------------------------------------------------------------------------------

newtype Rope v a = Rope (FingerTree (LineMeasure v) (Line a))
  deriving Show

type role Rope nominal nominal

instance (RelativeMonoid v, Measured v a, FromText a) => Semigroup (Rope v a) where
  Rope l <> Rope r = Rope $ case viewr l of
    EmptyR -> r
    ll :> Line m _
      | Text.lengthWord16 m > 0 -> case Text.last m of
        '\n' -> l >< r
        '\r' -> case viewl r of
          Line "\n" _ :< rr -> ll >< (fromText (snoc m '\n') <| rr) -- avoid associativity problems re the "\r\n" line terminator
          _               -> l >< r
        _ -> case viewl r of -- last line in the first rope is incomplete
          Line n _ :< rr -> ll >< (fromText (mappend m n) <| rr)
          EmptyL -> l -- but there is no right rope
     | otherwise -> ll >< r

instance (RelativeMonoid v, Measured v a, FromText a) => Monoid (Rope v a) where
  mempty = Rope mempty
  mappend = (<>)

instance (RelativeMonoid v, Measured v a) => Measured (LineMeasure v) (Rope v a) where
  measure (Rope r) = measure r

instance (RelativeMonoid v, Measured v a, FromText a) => FromText (Rope v a) where
  fromText = foldLines step (Rope mempty) where
    step (Rope xs) x = Rope (xs |> fromText x)

instance (RelativeMonoid v, Measured v a, FromText a) => IsString (Rope v a) where
 fromString = fromText . pack

splitAtPosition :: (RelativeMonoid v, Measured v a, FromText a) => Position -> Rope v a -> (Rope v a, Rope v a)
splitAtPosition (Position lno cno) (Rope xs) = case search (\x _ -> lineCount x >= lno) xs of
  F.Position l lm@(Line m _) r
    | cno < Text.lengthWord16 m ->
      (Rope $ l |> fromText (Text.takeWord16 cno m), Rope $ fromText (Text.dropWord16 cno m) <| r)
    | otherwise -> (Rope (l |> lm), Rope r)
  F.OnLeft -> (Rope xs, mempty)
  F.OnRight -> (mempty, Rope xs)
  F.Nowhere -> error "splitAtPosition: nowhere"

splitAtDelta :: (RelativeMonoid v, Measured v a, FromText a) => Delta -> Rope v a -> (Rope v a, Rope v a)
splitAtDelta d (Rope xs) = case search (\x _ -> delta x >= d) xs of
  F.Position l (Line m _) r
      | !cno <- units d - units (measure l) ->
        (Rope $ l |> fromText (Text.takeWord16 cno m), Rope $ fromText (Text.dropWord16 cno m) <| r)
  F.OnLeft -> (Rope xs, mempty)
  F.OnRight -> (mempty, Rope xs)
  F.Nowhere -> error "splitAtDelta: nowhere"

insertAt :: (RelativeMonoid v, Measured v a, FromText a) => Position -> Text -> Rope v a -> Rope v a
insertAt p t rope = case splitAtPosition p rope of
  (l, r) -> l <> fromText t <> r

deleteRange :: (RelativeMonoid v, Measured v a, FromText a) => Range -> Rope v a -> Rope v a
deleteRange (Range lo hi) doc = case splitAtPosition hi doc of
  (m, r) -> fst (splitAtPosition lo m) <> r

replaceRange :: (RelativeMonoid v, Measured v a, FromText a) => Range -> Text -> Rope v a -> Rope v a
replaceRange (Range lo hi) t doc = case splitAtPosition hi doc of
  (m, r) -> fst (splitAtPosition lo m) <> fromText t <> r

-- | Compute an Language Server Protocol 'Position' from a 'Delta' given the associated text
--
-- Takes /O(log l)/ where l is the number of lines
deltaToPosition :: (RelativeMonoid v, Measured v a) => Rope v a -> Delta -> Position
deltaToPosition (Rope t) (Delta d) = case split (\x -> units x >= d) t of
  (l, _) | ml <- measure l -> Position (lineCount ml) (d - units ml)

-- | Convert from a Language Server Protocol 'Position' to a 'Delta' given the associated text.
--
-- /O(log l)/ where l is the number of lines
positionToDelta :: (RelativeMonoid v, Measured v a) => Rope v a -> Position -> Delta
positionToDelta (Rope t) (Position nl c16) = case split (\x -> lineCount x >= nl) t of
  (l, _) -> Delta $ units (measure l) + c16
