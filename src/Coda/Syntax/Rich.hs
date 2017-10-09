{-# language BangPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveDataTypeable #-}
{-# language FunctionalDependencies #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language DeriveAnyClass #-}
{-# language TypeFamilies #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-# language ExplicitNamespaces #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Syntax.Rich
  ( Pair, LocatedPair
  -- Tokens
  , MismatchError(..)
  , Rich(..)
  , AsRich(..)
  , pattern Rich
  ) where

import Control.Exception
import Control.Lens
import Coda.Relative.Cat as Cat
import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Located
import Data.Data
import GHC.Generics

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

data Rich a
  = Nested {-# unpack #-} !(LocatedPair a) !(Cat a)
  | Mismatch {-# unpack #-} !(MismatchError a) !(Cat a)
  | UnmatchedOpening {-# unpack #-} !(LocatedPair a)
  | UnmatchedClosing {-# unpack #-} !(LocatedPair a)
  | LexicalError {-# unpack #-} !Delta String

makeClassyPrisms ''Rich

pattern Rich :: AsRich t a => () => Rich a -> t
pattern Rich a <- (preview _Rich -> Just a) where
  Rich a = _Rich # a

deriving instance (Relative a, Show (Pair a), Show a) => Show (Rich a)
deriving instance (Relative a, Read (Pair a), Read a) => Read (Rich a)
deriving instance (Relative a, Eq (Pair a), Eq a) => Eq (Rich a)
deriving instance (Relative a, Ord (Pair a), Ord a) => Ord (Rich a)

instance Relative a => Relative (Rich a) where
  rel d (Nested dp xs) = Nested (rel d dp) (rel d xs)
  rel d (Mismatch e xs) = Mismatch  (rel d e) (rel d xs)
  rel d (UnmatchedOpening dp) = UnmatchedOpening (rel d dp)
  rel d (UnmatchedClosing dq) = UnmatchedClosing (rel d dq)
  rel d (LexicalError d' xs) = LexicalError (d+d') xs
