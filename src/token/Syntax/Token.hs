{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language DataKinds #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2019
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Syntax.Token
  ( Token(..)
  , Pair(..)
  , LayoutMode(..)
  , Keyword(..)
  , keywords
  , startingKeywords
  , layoutKeywords
  , nested
  , mismatch
  , unmatchedOpening
  , unmatchedClosing
  , lexicalError
  , reifyLayoutMode
  ) where

import Data.Data
import Data.Default
import Data.Reflection
import Data.Ix
import Data.Set as Set
import Data.Text (Text)
import GHC.Generics

import Relative.Cat
import Relative.Class
import Relative.Delta
import Relative.Located
import Syntax.Name

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

data Token
  = Token        {-# unpack #-} !Delta {-# unpack #-} !Text -- as yet uninterpreted lexemes
  | TokenName    {-# unpack #-} !Delta !Name
  | TokenKeyword {-# unpack #-} !Delta !Keyword
  | TokenInteger {-# unpack #-} !Delta !Integer
  | TokenDouble  {-# unpack #-} !Delta {-# unpack #-} !Double
  | TokenString  {-# unpack #-} !Delta {-# unpack #-} !Text
  | TokenChar    {-# unpack #-} !Delta {-# unpack #-} !Char
  | TokenNested  {-# unpack #-} !(Located Pair) !(Cat Token)
  | TokenMismatch {-# unpack #-} !(Located Pair) {-# unpack #-} !(Located Pair) !(Cat Token)
  | TokenUnmatchedOpening {-# unpack #-} !(Located Pair)
  | TokenUnmatchedClosing {-# unpack #-} !(Located Pair)
  | TokenLexicalError {-# unpack #-} !Delta String
  deriving (Eq,Ord,Show,Read)

nested :: Located Pair -> Cat Token -> Token
nested = TokenNested

mismatch :: Located Pair -> Located Pair -> Cat Token -> Token
mismatch = TokenMismatch

unmatchedOpening :: Located Pair -> Token
unmatchedOpening = TokenUnmatchedOpening

unmatchedClosing :: Located Pair -> Token
unmatchedClosing = TokenUnmatchedClosing

lexicalError :: Delta -> String -> Token
lexicalError = TokenLexicalError

instance Relative Token where
  rel 0 xs = xs
  rel d0 xs0 = go d0 xs0 where
    go d (Token d' t) = Token (d+d') t
    go d (TokenName d' n) = TokenName (d+d') n
    go d (TokenKeyword d' k) = TokenKeyword (d+d') k
    go d (TokenInteger d' i) = TokenInteger (d+d') i
    go d (TokenDouble d' f) = TokenDouble (d+d') f
    go d (TokenString d' l) = TokenString (d+d') l
    go d (TokenChar d' l) = TokenChar (d+d') l
    go d (TokenNested dp ts) = TokenNested (rel d dp) (rel d ts)
    go d (TokenMismatch dp dq ts) = TokenMismatch (rel d dp) (rel d dq) (rel d ts)
    go d (TokenUnmatchedOpening dp) = TokenUnmatchedOpening (rel d dp)
    go d (TokenUnmatchedClosing dp) = TokenUnmatchedClosing (rel d dp)
    go d (TokenLexicalError d' s) = TokenLexicalError (d+d') s

data Pair = Brace | Bracket | Paren
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Generic)

data LayoutMode = LNone | LDo | LLet | LOf | LWhere
  deriving (Eq,Ord,Show,Read)

instance Reifies 'LNone LayoutMode where reflect _ = LNone
instance Reifies 'LDo LayoutMode where reflect _ = LDo
instance Reifies 'LLet LayoutMode where reflect _ = LLet
instance Reifies 'LOf LayoutMode where reflect _ = LOf
instance Reifies 'LWhere LayoutMode where reflect _ = LWhere

reifyLayoutMode :: LayoutMode -> (forall (s :: LayoutMode). Reifies s LayoutMode => Proxy s -> r) -> r
reifyLayoutMode LNone f = f (Proxy :: Proxy 'LNone)
reifyLayoutMode LDo f = f (Proxy :: Proxy 'LDo)
reifyLayoutMode LLet f = f (Proxy :: Proxy 'LLet)
reifyLayoutMode LOf f = f (Proxy :: Proxy 'LOf)
reifyLayoutMode LWhere f = f (Proxy :: Proxy 'LWhere)

instance Default LayoutMode where
  def = LNone
