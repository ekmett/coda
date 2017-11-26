{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Sudoku.Cell (Cell(..)) where

import Prelude hiding ((&&), (||), not, and, or, all, any)

import Data.Typeable (Typeable)
import Data.Word
import Ersatz
import GHC.Generics

newtype Cell = Cell Bit4
  deriving (Show, Typeable, Generic)

instance Boolean   Cell
instance Variable  Cell
instance Equatable Cell

instance Codec Cell where
  type Decoded Cell = Word8
  decode s (Cell b) = decode s b
  encode n | 1 <= n && n <= 9 = Cell (encode n)
           | otherwise = error ("Cell encode: invalid value " ++ show n)
