{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) David Feuer 2016
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Coda.Util.BitQueue
    ( BitQueue
    , BitQueueB
    , emptyQB
    , snocQB
    , buildQ
    , unconsQ
    , toListQ
    ) where

import Coda.Util.Bits (shiftLL, shiftRL, wordSize)
import Data.Bits ((.|.), (.&.), testBit, countTrailingZeros)

-- A bit queue builder. We represent a double word using two words
-- because we don't currently have access to proper double words.
data BitQueueB = BQB {-# UNPACK #-} !Word
                     {-# UNPACK #-} !Word

newtype BitQueue = BQ BitQueueB deriving Show

-- Intended for debugging.
instance Show BitQueueB where
  show (BQB hi lo) = "BQ"++
    show (map (testBit hi) [(wordSize - 1),(wordSize - 2)..0]
            ++ map (testBit lo) [(wordSize - 1),(wordSize - 2)..0])

-- | Create an empty bit queue builder. This is represented as a single guard
-- bit in the most significant position.
emptyQB :: BitQueueB
emptyQB = BQB (shiftLL 1 (wordSize - 1)) 0
{-# INLINE emptyQB #-}

-- Shift the double word to the right by one bit.
shiftQBR1 :: BitQueueB -> BitQueueB
shiftQBR1 (BQB hi lo) = BQB hi' lo' where
  lo' = shiftRL lo 1 .|. shiftLL hi (wordSize - 1)
  hi' = shiftRL hi 1
{-# INLINE shiftQBR1 #-}

-- | Enqueue a bit. This works by shifting the queue right one bit,
-- then setting the most significant bit as requested.
{-# INLINE snocQB #-}
snocQB :: BitQueueB -> Bool -> BitQueueB
snocQB bq b = case shiftQBR1 bq of
  BQB hi lo -> BQB (hi .|. shiftLL (fromIntegral (fromEnum b)) (wordSize - 1)) lo

-- | Convert a bit queue builder to a bit queue. This shifts in a new
-- guard bit on the left, and shifts right until the old guard bit falls
-- off.
{-# INLINE buildQ #-}
buildQ :: BitQueueB -> BitQueue
buildQ (BQB hi 0) = BQ (BQB 0 lo') where
  zeros = countTrailingZeros hi
  lo' = shiftRL (shiftRL hi 1 .|. shiftLL 1 (wordSize - 1)) zeros
buildQ (BQB hi lo) = BQ (BQB hi' lo') where
  zeros = countTrailingZeros lo
  lo1 = shiftRL lo 1 .|. shiftLL hi (wordSize - 1)
  hi1 = shiftRL hi 1 .|. shiftLL 1 (wordSize - 1)
  lo' = shiftRL lo1 zeros .|. shiftLL hi1 (wordSize - zeros)
  hi' = shiftRL hi1 zeros

-- Test if the queue is empty, which occurs when theres
-- nothing left but a guard bit in the least significant
-- place.
nullQ :: BitQueue -> Bool
nullQ (BQ (BQB 0 1)) = True
nullQ _ = False
{-# INLINE nullQ #-}

-- | Dequeue an element, or discover the queue is empty.
unconsQ :: BitQueue -> Maybe (Bool, BitQueue)
unconsQ q | nullQ q = Nothing
unconsQ (BQ bq@(BQB _ lo)) = Just (hd, BQ tl) where
  !hd = (lo .&. 1) /= 0
  !tl = shiftQBR1 bq
{-# INLINE unconsQ #-}

-- | Convert a bit queue to a list of bits by unconsing.
-- This is used to test that the queue functions properly.
toListQ :: BitQueue -> [Bool]
toListQ bq = case unconsQ bq of
  Nothing -> []
  Just (hd, tl) -> hd : toListQ tl
