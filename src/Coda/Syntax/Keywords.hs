{-# language OverloadedLists #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

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
  , Keyword(..)
  ) where

import Data.Data
import Data.Ix
import Data.Set as Set
import GHC.Generics

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

data Keyword
  = KAs
  | KCase
  | KClass
  | KData
  | KDefault
  | KDeriving
  | KDo
  | KElse
  | KHiding
  | KIf
  | KImport
  | KIn
  | KInfix
  | KInfixl
  | KInfixr
  | KInstance
  | KLet
  | KModule
  | KNewtype
  | KOf
  | KQualified
  | KThen
  | KType
  | KWhere
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)
