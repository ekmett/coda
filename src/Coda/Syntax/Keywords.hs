module Coda.Syntax.Keywords
  ( keywords
  , startingKeywords
  , layoutKeywords
  ) where

import Data.Set as Set

startingKeywords :: Set String
startingKeywords = Set.fromList
  ["data","import","infix","infixl","infixr","module","newtype","type"]

keywords :: Set String
keywords = Set.fromList 
  ["qualified","case","in"]

layoutKeywords :: Set String
layoutKeywords = Set.fromList  
  ["do","let","of","where"]
