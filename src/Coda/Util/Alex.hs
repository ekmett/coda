{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Util.Alex
  ( AlexInput(..)
  , alexGetByte
  ) where

import Coda.Relative.Delta
import Data.Bits
import Data.String
import Data.Text
import Data.Text.Unsafe as Text
import Data.Word (Word8)
import Prelude hiding ((!!))

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLists
-- >>> import Data.List as List (unfoldr)

-- |
-- Invariants:
--
-- @
-- 'delta' >= 0
-- @
data AlexInput = AlexInput
  { alexInputState    :: {-# unpack #-} !Int
  , alexInputPrevChar :: {-# unpack #-} !Char
  , alexInputDelta    :: {-# unpack #-} !Int
  , alexInputText     :: {-# unpack #-} !Text
  }

instance HasDelta AlexInput where
  delta = alexInputDelta

instance IsString AlexInput where
  fromString = AlexInput 0 '\n' 0 . fromString

emit :: Int -> Int -> Int -> Text -> Int -> (Word8, AlexInput)
emit !s !p !d !t !w = (b, i) where
  !b = fromIntegral w
  !i = AlexInput s (toEnum p) d t
{-# inline emit #-}

-- |
-- >>> List.unfoldr alexGetByte "hello world"
-- [104,101,108,108,111,32,119,111,114,108,100]
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput !s (fromEnum -> !p) !d !t) = case s of
  3 -> Just $! emit 2 p d t $! 0x80 + unsafeShiftR p 12 .&. 0x3f
  2 -> Just $! emit 1 p d t $! 0x80 + unsafeShiftR p 6  .&. 0x3f
  1 -> Just $! emit 0 p d t $! 0x80 + p                 .&. 0x3f
  _ | d < Text.lengthWord16 t -> Just $! case Text.iter t d of
      Text.Iter (fromEnum -> !i) d'
        | i <= 0x7f   -> emit 0 i d' t i
        | i <= 0x7ff  -> emit 1 i d' t $! 0xc0 + unsafeShiftR p 6
        | i <= 0xffff -> emit 2 i d' t $! 0xe0 + unsafeShiftR p 12
        | otherwise   -> emit 3 i d' t $! 0xf0 + unsafeShiftR p 18
    | otherwise -> Nothing
