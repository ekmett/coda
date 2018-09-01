{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A simple input adapter that allows @alex@ to work with 'Text'
---------------------------------------------------------------------------------

module Syntax.Alex
  ( AlexInput(..)
  , AlexInputState(..)
  , alexGetByte
  ) where

import Data.Bits
import Data.Data
import Data.Hashable
import Data.String
import Data.Text
import Data.Text.Unsafe as Text
import Data.Word (Word8)
import GHC.Generics

import Syntax.FromText

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLists
-- >>> import Data.List as List (unfoldr)

data AlexInputState
  = S0 | S1 | S2 | S3
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable AlexInputState

-- |
-- Invariants:
--
-- @
-- 'delta' >= 0
-- @
data AlexInput = AlexInput
  { alexInputState    :: !AlexInputState
  , alexInputPrevChar :: {-# unpack #-} !Char
  , alexInputDelta    :: {-# unpack #-} !Int
  , alexInputText     :: {-# unpack #-} !Text
  } deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable AlexInput

instance IsString AlexInput where
  fromString = AlexInput S0 '\n' 0 . fromString

instance FromText AlexInput where
  fromText = AlexInput S0 '\n' 0
  {-# inline conlike fromText #-}

ok :: a -> b -> Maybe (a,b)
ok !a !b = Just (a,b)

-- |
-- >>> Prelude.take 20 $ List.unfoldr alexGetByte "hello world"
-- [104,101,108,108,111,32,119,111,114,108,100]
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput !s !c !d !t) = case s of
  S3 | i <- fromEnum c -> ok (fromIntegral $ 0x80 + unsafeShiftR i 12 .&. 0x3f) (AlexInput S2 c (d+1) t)
  S2 | i <- fromEnum c -> ok (fromIntegral $ 0x80 + unsafeShiftR i 6  .&. 0x3f) (AlexInput S1 c (d+1) t)
  S1 | i <- fromEnum c -> ok (fromIntegral $ 0x80 + i .&. 0x3f)                 (AlexInput S0 c (d+1) t)
  S0 | d < Text.lengthWord16 t -> case Text.iter t d of
      Text.Iter c' d'
        | i' <= 0x7f   -> ok (fromIntegral i')                          (AlexInput S0 c' (d+d') t)
        | i' <= 0x7ff  -> ok (fromIntegral $ 0xc0 + unsafeShiftR i' 6)  (AlexInput S1 c' (d+d') t)
        | i' <= 0xffff -> ok (fromIntegral $ 0xe0 + unsafeShiftR i' 12) (AlexInput S2 c' (d+d') t)
        | otherwise    -> ok (fromIntegral $ 0xf0 + unsafeShiftR i' 18) (AlexInput S3 c' (d+d') t)
        where i' = fromEnum c'
    | otherwise -> Nothing
