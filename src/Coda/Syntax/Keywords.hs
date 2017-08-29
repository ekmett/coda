{-# language OverloadedLists #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Coda.Syntax.Keywords
  ( keywords
  , startingKeywords
  , layoutKeywords
  ) where

import Data.Set as Set

-- | these are keywords that are only valid at the start of a top level statement
startingKeywords :: Set String
startingKeywords = ["data","import","infix","infixl","infixr","module","newtype","type"]

-- | These are keywords that may occur anywhere in a source file
keywords :: Set String
keywords = ["qualified","case","in"]

-- | These are keywords that introduce layout
layoutKeywords :: Set String
layoutKeywords = ["do","let","of","where"]
