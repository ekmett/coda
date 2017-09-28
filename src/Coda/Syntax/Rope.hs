{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
module Coda.Syntax.Rope
  ( Rope(..)
  , fromText
  , splitAtPosition
  , splitAtDelta
  , insertAt
  , deleteRange
  , replaceRange
  ) where

import Coda.Relative.Delta
import Coda.Syntax.Line
import Data.FingerTree
import Data.Semigroup
import Data.String
import Data.Text as Text hiding (split)
import Data.Text.Unsafe as Text
import Language.Server.Protocol

-- this will need to evolve quite a bit

newtype Rope = Rope (FingerTree LineMeasure Line)

instance Semigroup Rope where
  Rope l <> Rope r = Rope $ case viewr l of
    EmptyR -> r
    ll :> Line m 
      | Text.lengthWord16 m > 0 -> case Text.last m of
        '\n' -> l >< r
        '\r' -> case viewl r of
          Line "\n" :< rr -> ll >< (Line (snoc m '\n') <| rr) -- avoid associativity problems re the "\r\n" line terminator
          _               -> l >< r
        _ -> case viewl r of -- last line in the first rope is incomplete
          Line n :< rr -> ll >< (Line (mappend m n) <| rr)
          EmptyL -> l -- but there is no right rope
     | otherwise -> ll >< r

instance Monoid Rope where
  mempty = Rope mempty
  mappend = (<>)

instance Measured LineMeasure Rope where
  measure (Rope r) = measure r

fromText :: Text -> Rope
fromText = foldLines step (Rope mempty) where
  step (Rope xs) x = Rope (xs |> Line x)

instance IsString Rope where
 fromString = fromText . pack

splitAtPosition :: Position -> Rope -> (Rope, Rope)
splitAtPosition (Position lno cno) (Rope xs) = case split (\x -> lineCount x >= lno) xs of
  (l, r) -> case viewr l of
    ll :> Line m
      | cno < Text.lengthWord16 m -> (Rope $ ll |> Line (Text.takeWord16 cno m), Rope $ Line (Text.dropWord16 cno m) <| r)
      | otherwise -> (Rope l, Rope r)
    EmptyR -> (Rope l, Rope r) -- out of bounds

splitAtDelta :: Delta -> Rope -> (Rope, Rope)
splitAtDelta d (Rope xs) = case split (\x -> delta x >= d) xs of
  (l, r) -> case viewl r of
    EmptyL -> (Rope l, Rope mempty)
    Line m :< rr
      | cno <- units d - units (measure l) -> (Rope $ l |> Line (Text.takeWord16 cno m), Rope $ Line (Text.dropWord16 cno m) <| rr)

insertAt :: Position -> Text -> Rope -> Rope
insertAt p t rope = case splitAtPosition p rope of
  (l, r) -> l <> fromText t <> r

deleteRange :: Range -> Rope -> Rope
deleteRange (Range lo hi) doc = case splitAtPosition hi doc of
  (m, r) -> fst (splitAtPosition lo m) <> r

replaceRange :: Range -> Text -> Rope -> Rope
replaceRange (Range lo hi) t doc = case splitAtPosition hi doc of
  (m, r) -> fst (splitAtPosition lo m) <> fromText t <> r
