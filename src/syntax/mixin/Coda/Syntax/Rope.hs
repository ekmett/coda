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

import Coda.Relative.Delta
import Coda.Syntax.Dyck
import Coda.Syntax.FromText
import Coda.Syntax.Lexer
import Coda.Syntax.Summary
import Control.Lens
import Data.Data
import Data.Default
import qualified Coda.FingerTree as F
import Coda.FingerTree hiding (SearchResult(..))
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
import Prelude hiding (lex)


data Line = Line { runLine :: {-# unpack #-} !Text, content :: {-# unpack #-} !Dyck }
  deriving (Eq, Ord, Show, Read)

instance Hashable Line where
  hashWithSalt e = hashWithSalt e . runLine
  {-# inline hashWithSalt #-}
  hash = hash . runLine
  {-# inline hash #-}

instance FromText Line where
  fromText t = Line t (lex t)

instance IsString Line where
  fromString = fromText . fromString

instance HasDelta Line where
  delta = Delta #. Text.lengthWord16 . runLine

instance Default Line where
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
-- Also when used to measure a 'FingerTree' you can use the 'FingerTree' to convert
-- back and forth between 'Delta' and 'Position' in /O(log l)/ time, while still
-- letting us use the compact single-integer Abelian group 'Delta' representation
-- internally for almost all positioning.
--
data LineMeasure = LineMeasure {-# unpack #-} !Int {-# unpack #-} !Delta {-# unpack #-} !Summary
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance HasDelta LineMeasure where
  delta (LineMeasure _ d _) = d

instance Hashable LineMeasure

instance Semigroup LineMeasure where
  LineMeasure l d v <> LineMeasure l' d' v' = LineMeasure (l + l') (d + d') (mergeSummary l d v l' d' v')

instance Monoid LineMeasure where
  mempty = LineMeasure 0 0 def
  mappend = (<>)

-- Measured a is somewhat the wrong constraint here
instance Measured Line where
  type Measure Line = LineMeasure
  measure (Line l a) = LineMeasure 1 (Delta $ Text.lengthWord16 l) (summarize l a)

instance Default LineMeasure where
  def = LineMeasure 0 0 def

--------------------------------------------------------------------------------
-- HasLineMeasure
--------------------------------------------------------------------------------

class HasDelta t => HasLineMeasure t where
  lineMeasure :: t -> LineMeasure

  lineCount :: t -> Int
  lineCount = lineCount . lineMeasure

instance HasLineMeasure LineMeasure where
  lineCount (LineMeasure l _ _) = l
  lineMeasure = id

-- instance (RelativeMonoid (Measure a), Measured a) => HasLineMeasure (Line a) (Measure a) where
--  lineMeasure = measure

--------------------------------------------------------------------------------
-- Ropes
--------------------------------------------------------------------------------

newtype Rope = Rope (FingerTree Line)
  deriving Show

instance Semigroup Rope where
  Rope l <> Rope r = Rope $ case l of
    ll :> Line m _
      | Text.lengthWord16 m > 0 -> case Text.last m of
        '\n' -> l <> r
        '\r' -> case r of
          Line "\n" _ :< rr -> ll <> (fromText (Text.snoc m '\n') <| rr) -- avoid associativity problems re the "\r\n" line terminator
          _ -> l <> r
        _ -> case r of -- last line in the first rope is incomplete
          Line n _ :< rr -> ll <> (fromText (mappend m n) <| rr)
          _ -> l -- but there is no right rope
     | otherwise -> ll <> r
    _ -> r

instance Monoid Rope where
  mempty = Rope mempty
  mappend = (<>)

instance Measured Rope where
  type Measure Rope = LineMeasure
  measure (Rope r) = measure r

instance FromText Rope where
  fromText = foldLines step (Rope mempty) where
    step (Rope xs) x = Rope (xs |> fromText x)

instance IsString Rope where
 fromString = fromText . pack

splitAtPosition :: Position -> Rope -> (Rope, Rope)
splitAtPosition (Position lno cno) (Rope xs) = case search (\x _ -> lineCount x >= lno) xs of
  F.Position l lm@(Line m _) r
    | cno < Text.lengthWord16 m ->
      (Rope $ l |> fromText (Text.takeWord16 cno m), Rope $ fromText (Text.dropWord16 cno m) <| r)
    | otherwise -> (Rope (l |> lm), Rope r)
  F.OnLeft -> (Rope xs, mempty)
  F.OnRight -> (mempty, Rope xs)
  F.Nowhere -> error "splitAtPosition: nowhere"

splitAtDelta :: Delta -> Rope -> (Rope, Rope)
splitAtDelta d (Rope xs) = case search (\x _ -> delta x >= d) xs of
  F.Position l (Line m _) r
      | !cno <- units d - units (measure l) ->
        (Rope $ l |> fromText (Text.takeWord16 cno m), Rope $ fromText (Text.dropWord16 cno m) <| r)
  F.OnLeft -> (Rope xs, mempty)
  F.OnRight -> (mempty, Rope xs)
  F.Nowhere -> error "splitAtDelta: nowhere"

insertAt :: Position -> Text -> Rope -> Rope
insertAt p t rope = case splitAtPosition p rope of
  (l, r) -> l <> fromText t <> r

deleteRange :: Range -> Rope -> Rope
deleteRange (Range lo hi) doc = case splitAtPosition hi doc of
  (m, r) -> fst (splitAtPosition lo m) <> r

replaceRange :: Range -> Text -> Rope -> Rope
replaceRange (Range lo hi) t doc = case splitAtPosition hi doc of
  (m, r) -> fst (splitAtPosition lo m) <> fromText t <> r

-- | Compute an Language Server Protocol 'Position' from a 'Delta' given the associated text
--
-- Takes /O(log l)/ where l is the number of lines
deltaToPosition :: Rope -> Delta -> Position
deltaToPosition (Rope t) (Delta d) = case split (\x -> units x >= d) t of
  (l, _) | ml <- measure l -> Position (lineCount ml) (d - units ml)

-- | Convert from a Language Server Protocol 'Position' to a 'Delta' given the associated text.
--
-- /O(log l)/ where l is the number of lines
positionToDelta :: Rope -> Position -> Delta
positionToDelta (Rope t) (Position nl c16) = case split (\x -> lineCount x >= nl) t of
  (l, _) -> Delta $ units (measure l) + c16
