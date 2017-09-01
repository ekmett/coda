{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Clark Gaebel 2012
--                (c) Johan Tibel 2012
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
-----------------------------------------------------------------------------
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.

module Coda.Util.Bits
  ( highestBitMask
  , shiftLL
  , shiftRL
  , wordSize
  ) where

import Data.Bits ((.|.), xor)
import Data.Bits (finiteBitSize)
import GHC.Exts (Word(..), Int(..))
import GHC.Prim (uncheckedShiftL#, uncheckedShiftRL#)

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
highestBitMask x1 = let x2 = x1 .|. x1 `shiftRL` 1
                        x3 = x2 .|. x2 `shiftRL` 2
                        x4 = x3 .|. x3 `shiftRL` 4
                        x5 = x4 .|. x4 `shiftRL` 8
                        x6 = x5 .|. x5 `shiftRL` 16
                        x7 = x6 .|. x6 `shiftRL` 32
                     in x7 `xor` (x7 `shiftRL` 1)
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
