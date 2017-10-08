{-# language BangPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language TypeFamilies #-}
{-# language StandaloneDeriving #-}
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

module Coda.Syntax.Token
  ( Pair, LocatedPair
  -- Tokens
  , MismatchError(..)
  , Token(..)
  ) where

import Control.Exception
import Coda.Relative.Cat as Cat
import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Located
import Data.Data
import GHC.Generics hiding (from)
import Prelude hiding (lex)

--------------------------------------------------------------------------------
-- Rich Tokens
--------------------------------------------------------------------------------

data family Pair (a :: *) :: *
type LocatedPair a = Located (Pair a)

data MismatchError a
  = MismatchError {-# unpack #-} !(LocatedPair a) {-# unpack #-} !(LocatedPair a)

deriving instance Generic (MismatchError a)
deriving instance Show (Pair a) => Show (MismatchError a)
deriving instance Read (Pair a) => Read (MismatchError a)
deriving instance Eq (Pair a) => Eq (MismatchError a)
deriving instance Ord (Pair a) => Ord (MismatchError a)
deriving instance (Data (Pair a), Data a) => Data (MismatchError a)

instance (Show (Pair a), Typeable a) => Exception (MismatchError a)

instance Relative (MismatchError a) where
  rel d (MismatchError l r) = MismatchError (rel d l) (rel d r)

data Token a
  = Token !a
  | Nested {-# unpack #-} !(LocatedPair a) !(Cat (Token a))
  | Mismatch {-# unpack #-} !(MismatchError a) !(Cat (Token a))
  | UnmatchedOpening {-# unpack #-} !(LocatedPair a)
  | UnmatchedClosing {-# unpack #-} !(LocatedPair a)
  | LexicalError {-# unpack #-} !Delta

deriving instance (Relative a, Show (Pair a), Show a) => Show (Token a)
deriving instance (Relative a, Read (Pair a), Read a) => Read (Token a)
deriving instance (Relative a, Eq (Pair a), Eq a) => Eq (Token a)
deriving instance (Relative a, Ord (Pair a), Ord a) => Ord (Token a)

instance Relative a => Relative (Token a) where
  rel d (Token a) = Token (rel d a)
  rel d (Nested dp xs) = Nested (rel d dp) (rel d xs)
  rel d (Mismatch e xs) = Mismatch  (rel d e) (rel d xs)
  rel d (UnmatchedOpening dp) = UnmatchedOpening (rel d dp)
  rel d (UnmatchedClosing dq) = UnmatchedClosing (rel d dq)
  rel d (LexicalError d') = LexicalError (d+d')
