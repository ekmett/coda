{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2017-2018 (c) Clark Gaebel 2012, (c) Johan Tibel 2012
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Portability :  non-portable
-----------------------------------------------------------------------------

module Util.Bits
  ( highestBitMask
  , shiftLL
  , shiftRL
  , wordSize
  ) where

import Data.Bits ((.|.), xor, finiteBitSize)
import GHC.Exts (Word(..), Int(..))
import GHC.Prim (uncheckedShiftL#, uncheckedShiftRL#)

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
highestBitMask x1 = x7 `xor` (x7 `shiftRL` 1) where
  x2 = x1 .|. shiftRL x1 1
  x3 = x2 .|. shiftRL x2 2
  x4 = x3 .|. shiftRL x3 4
  x5 = x4 .|. shiftRL x4 8
  x6 = x5 .|. shiftRL x5 16
  x7 = x6 .|. shiftRL x6 32
{-# inline highestBitMask #-}

-- Right and left logical shifts.
shiftRL, shiftLL :: Word -> Int -> Word
shiftRL (W# x) (I# i) = W# (uncheckedShiftRL# x i)
shiftLL (W# x) (I# i) = W# (uncheckedShiftL#  x i)
{-# inline CONLIKE shiftRL #-}
{-# inline CONLIKE shiftLL #-}

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)
{-# inline wordSize #-}
