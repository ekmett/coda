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
startingKeywords =
  [ "class", "data", "default", "import", "infix", "infixl"
  , "infixr", "instance", "module", "newtype", "type"
  ]

-- | These are keywords that may occur anywhere in a source file
keywords :: Set String
keywords = ["as", "case", "deriving", "else" , "hiding", "if", "in", "qualified", "then" ]

-- | These are keywords that introduce layout
layoutKeywords :: Set String
layoutKeywords = ["do","let","of","where"]
