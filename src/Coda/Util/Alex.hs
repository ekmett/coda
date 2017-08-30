{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}

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
  , alexInputPrevChar
  , skip
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Syntax.Line
import Coda.Util.Primitive
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
  { alexInputDelta :: {-# unpack #-} !Int
  , alexInputLine  :: !Line
  } deriving Show

instance HasDelta AlexInput where
  delta = alexInputDelta

instance HasLine AlexInput where
  line = alexInputLine

-- |
-- >>> alexInputPrevChar $ AlexInput 0 "a"
-- '\n'
--
-- >>> alexInputPrevChar $ AlexInput 1 "a"
-- 'a'
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput b l)
  = if b <= 0 then '\n'                                     else let !t0 = fromIntegral $ l!!(b-1)                             in
    if hb t0  then toEnum (t0 .&. 0x7f)                     else let !t0' = t0 .&. 0x3f                         in
    if b <= 1 then backtrackingError                        else let !t1 = fromIntegral $ l!!(b-2)                             in
    if hb t1  then toEnum (unsafeShiftL (t1-192) 6 + t0')   else let !t1' = unsafeShiftL (t1 .&. 0x3f) 6 + t0'  in
    if b <= 2 then backtrackingError                        else let !t2 = fromIntegral $ l!!(b-3)                             in
    if hb t2  then toEnum (unsafeShiftL (t2-224) 12 + t1')  else let !t2' = unsafeShiftL (t2 .&. 0x3f) 12 + t1' in
    if b <= 3 then backtrackingError                        else toEnum (unsafeShiftL (fromIntegral (l!!(b-4))-240) 18 + t2')
  where hb x = x .&. 0xc0 /= 0x70 -- non 10xxxxxx UTF-8 tailbyte

skip :: Int -> AlexInput -> AlexInput
skip n (AlexInput d l) = AlexInput (n + d) l

backtrackingError :: a
backtrackingError = error "alexGetPrevChar: backtracking error"

-- |
-- Ideally we'd simply provide a monoid 'Delta' that acts on 'AlexInput'
-- but @alex@ isn't that sophisticated.
--
-- >>> List.unfoldr alexGetByte $ AlexInput mempty "hello world"
-- [104,101,108,108,111,32,119,111,114,108,100]
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput d l)
  | d < delta l, !w8 <- l !! d = justPair' w8 (AlexInput (d+1) l)
  | otherwise = Nothing
{-# inline alexGetByte #-}

justPair' :: a -> b -> Just (a, b)
justPair' !a !b = Just (a,b)
{-# inline justPair' #-}
