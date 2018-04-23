{-# language TypeFamilies #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}

module Syntax.Located 
  ( Located(..)
  ) where

import Control.Comonad
import Data.List (uncons)
import Text.Megaparsec.Stream
import Text.Megaparsec.Pos

-- megaparsec doesn't give the stream to anything useful here, so let's duplicate some effort =(
data Located a = Located !SourcePos a !SourcePos
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance Comonad Located where
  extract (Located _ a _) = a
  duplicate w@(Located p _ q) = Located p w q

instance Ord a => Stream [Located a] where -- Ord? really?
  type Token [Located a] = Located a
  type Tokens [Located a] = [Located a]
  tokenToChunk _ t = [t]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  positionAt1 _ _ (Located p _ _) = p
  positionAtN _ s [] = s
  positionAtN _ _ (Located p _ _:_) = p
  advance1 _ _ _ (Located _ _ q) = q
  advanceN _ _ s [] = s
  advanceN _ _ _ xs = case last xs of Located _ _ q -> q
  take1_ = uncons
  takeN_ n s
    | n <= 0 = Just ([],s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
