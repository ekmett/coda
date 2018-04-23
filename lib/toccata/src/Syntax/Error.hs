{-# language BangPatterns #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

module Syntax.Error 
  ( ppError
  ) where

import Data.Maybe (isNothing)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as E
import Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Megaparsec.Error
import Text.Megaparsec.Stream
import Text.Megaparsec.Pos
import Syntax.Located

ppError :: (Ord a, ShowToken (Located a), ShowErrorComponent e) => Text -> ParseError (Located a) e -> Doc AnsiStyle
ppError s e = vsep
  [ sourcePosStackPrettier (errorPos e) <> ":"
  , pretty padding     <> bar 
  , pretty lineNumber <+> bar <+> rline
  , pretty padding     <> bar <+> pretty rpadding <> cursor
  , parseErrorTextPrettier e
  ] where
  bar = annotate (color Yellow) "|"
  cursor = annotate (color Green) "^" -- TODO: extend as a span to the right
  epos = NE.last (errorPos e)
  lineNumber = show $ unPos $ sourceLine epos
  padding    = spaces $ Prelude.length lineNumber + 1
  rpadding   = spaces $ unPos (sourceColumn epos) - 1
  spaces n = justifyLeft n ' ' ""
  rline = case rline' of
    [] -> annotate (color Red) "<empty line>"
    xs -> pretty $ expandTab (mkPos 8) xs
  rline'     = fmap tokenAsChar . chunkToTokens (Proxy :: Proxy Text) $ selectLine (sourceLine epos) s

selectLine
  :: Pos  -- ^ Number of line to select
  -> Text -- ^ Input stream
  -> Text -- ^ Selected line
selectLine l = go pos1 where
  go !n !s
    | n == l    = fst (takeWhile_ notNewline s)
    | otherwise = go (n <> pos1) (stripNewline $ snd (takeWhile_ notNewline s))
  notNewline = not . tokenIsNewline
  stripNewline s = maybe s snd (take1_ s)

expandTab :: Pos -> String -> String
expandTab w' = go 0 where
  go 0 []        = []
  go 0 ('\t':xs) = go w xs
  go 0 (x:xs)    = x : go 0 xs
  go !n xs       = ' ' : go (n - 1) xs
  w              = unPos w'

parseErrorTextPrettier :: (Ord t, ShowToken t, ShowErrorComponent e)
  => ParseError t e -- ^ Parse error to render
  -> Doc AnsiStyle -- ^ Result of rendering
parseErrorTextPrettier (TrivialError _ us ps) =
  if isNothing us && E.null ps
    then "unknown parse error\n"
    else messageItemsPrettier "unexpected " (maybe E.empty E.singleton us) <>
         messageItemsPrettier "expecting " ps
parseErrorTextPrettier (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error\n"
    else vsep (pretty . showErrorComponent <$> E.toAscList xs)

messageItemsPrettier :: ShowErrorComponent a
  => Doc AnsiStyle     -- ^ Prefix to prepend
  -> E.Set a           -- ^ Collection of messages
  -> Doc AnsiStyle     -- ^ Result of rendering
messageItemsPrettier prefix ts
  | E.null ts = mempty
  | otherwise =
    let f = orList . NE.fromList . fmap pretty . E.toAscList . E.map showErrorComponent
    in prefix <> f ts <> "\n"

orList :: NonEmpty (Doc a) -> Doc a
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = sep (List.intersperse ", " (NE.init xs)) <> ", or " <> NE.last xs

sourcePosStackPrettier :: NonEmpty SourcePos -> Doc AnsiStyle
sourcePosStackPrettier ms = mconcat (f <$> rest) <> sourcePosPrettier pos
  where
    pos :| rest' = ms
    rest = Prelude.reverse rest'
    f p = "in file included from " <> sourcePosPrettier p <> ",\n"

sourcePosPrettier :: SourcePos -> Doc AnsiStyle
sourcePosPrettier (SourcePos n l c)
  | Prelude.null n = annotate bold showLC
  | otherwise = annotate bold $ pretty n <> ":" <> showLC
  where showLC = pretty $ show (unPos l) <> ":" <> show (unPos c)
