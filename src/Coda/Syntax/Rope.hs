{-# language RoleAnnotations #-}
{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

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
  , fromText
  , splitAtPosition
  , splitAtDelta
  , insertAt
  , deleteRange
  , replaceRange
  , deltaToPosition
  , positionToDelta
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Syntax.Line
import Data.FingerTree
import Data.Semigroup
import Data.String
import Data.Text as Text hiding (split)
import Data.Text.Unsafe as Text
import Language.Server.Protocol

-- this will need to evolve quite a bit

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
splitAtPosition (Position lno cno) (Rope xs) = case split (\x -> lineCount x >= lno) xs of
  (l, r) -> case viewr l of
    ll :> Line m _
      | cno < Text.lengthWord16 m -> (Rope $ ll |> fromText (Text.takeWord16 cno m), Rope $ fromText (Text.dropWord16 cno m) <| r)
      | otherwise -> (Rope l, Rope r)
    EmptyR -> (Rope l, Rope r) -- out of bounds

splitAtDelta :: (RelativeMonoid v, Measured v a, FromText a) => Delta -> Rope v a -> (Rope v a, Rope v a)
splitAtDelta d (Rope xs) = case split (\x -> delta x >= d) xs of
  (l, r) -> case viewl r of
    EmptyL -> (Rope l, Rope mempty)
    Line m _ :< rr
      | cno <- units d - units (measure l) -> (Rope $ l |> fromText (Text.takeWord16 cno m), Rope $ fromText (Text.dropWord16 cno m) <| rr)

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

