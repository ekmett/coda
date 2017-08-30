{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Syntax.Line
  (
  -- * Lines
    Line(..)
  , HasLine(..)
  , (!!)
  , encodeLine
  , EncodingError(..)
  -- * Summarizing Lines
  , LineMeasure(..)
  , HasLineMeasure(..)
  -- * Position <-> Delta
  , positionToDelta
  , deltaToPosition
  ) where

import Coda.Message.Language (Position(..))
import Coda.Relative.Delta
import Coda.Relative.Class
import Coda.Data.List as List
import Coda.Util.Primitive
import Control.Lens (AsEmpty(..), prism)
import Control.Monad.ST
import Data.Bits
import Data.ByteString.Internal
import Data.Data
import Data.FingerTree
import Data.Function (on)
import Data.Hashable
import Data.Primitive.ByteArray
import Data.Semigroup
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Internal as Text
import Data.Text.Internal (Text(..))
import qualified Data.Text.Array as Text
import GHC.Word
import GHC.Exts as Exts (IsList(..))
import GHC.Generics
import Prelude hiding ((!!))
import Unsafe.Coerce

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLists

-- | Invariants
--
-- * The only occurrences of '\n', '\r' or "\r\n" are at the end of the ByteArray
--
-- * Valid UTF-8 encoding
--
-- TODO: store a flag if the line is ASCII? Common enough in code.
--
-- TODO: Store in the ByteArray' a 'Word32' length, UTF-8 content, then pad to
-- integral multiple of 64, then store the middle two levels of a poppy index
-- interleaved. This would ensure that we only ever scan at most 512 bytes during
-- conversions of column counts, giving an O(1) conversion from byte count to code
-- unit count, and O(log c) conversion from code unit to byte count in exchange
-- for 8 bytes for every 2k in the line, and no index if the line is < 512
-- characters long. Kind of overkill at this point unless we start wanting to deal
-- with lots of parse errors in minified or machine generated pathological code.
data Line = Line ByteArray !(Relative.List EncodingError)

instance Eq Line where
  (==) = (==) `on` Exts.toList

instance Ord Line where
  compare = compare `on` Exts.toList

instance Hashable Line where
  hashWithSalt s l = hashWithSalt s (Exts.toList l)

(!!) :: Line -> Int -> Word8
Line ba _ !! i = indexByteArray ba i

instance Show Line where
  showsPrec d xs = showsPrec d $ unsafePackLenBytes (delta xs) $ Exts.toList xs

-- | An encoding error
data EncodingError = EncodingError !Int !Text
  deriving Show

instance HasDelta EncodingError where
  delta (EncodingError e _) = e

instance Relative EncodingError where
  rel l (EncodingError r e) = EncodingError (delta l + r) e

-- | Produce pre-lexer feedback when converting to a line
encodeLine :: Text -> ([EncodingError],Line)
encodeLine (Text (Text.Array tba0) toff0 tlen0) = runST $ do
    mba <- newByteArray (tlen0 * 3)
    go mba (ByteArray tba0) toff0 tlen0 0

  where
    write8 :: Integral a => MutableByteArray s -> Int -> a -> ST s ()
    write8 mba i a = writeByteArray mba i (fromIntegral a :: Word8)

    go :: MutableByteArray s -> ByteArray -> Int -> Int -> Int -> ST s (Either EncodingError Line)
    go !mba !_ !_ 0 !boff = do
      shrinkMutableByteArray mba boff
      Right . Line <$> unsafeFreezeByteArray mba
    go mba tba toff tlen boff
      | c < 0x80 = do
        write8 mba boff c
        go mba tba (toff+1) (tlen-1) (boff+1)
      | c < 0x800 = do
        write8 mba boff     $ 0xc0 + unsafeShiftR c 6
        write8 mba (boff+1) $ 0x80 + (c .&. 0x3f)
        go mba tba (toff+1) (tlen-1) (boff+2)
      | c < 0xd800 = do
        write8 mba boff     $ 0xe0 + unsafeShiftR c 12
        write8 mba (boff+1) $ 0x80 + (unsafeShiftR c 6 .&. 0x3f)
        write8 mba (boff+2) $ 0x80 + (c .&. 0x3f)
        go mba tba (toff+1) (tlen-1) (boff+3)
      | c < 0xdc00 =
        if tlen > 1
        then return $ Just (EncodingError boff "unpaired surrogate")
        else let !c2 = indexByteArray tba (toff+1) :: Word16 in
          if 0xdc00 <= c2 && c2 < 0xe000
          then return $ Just (EncodingError boff "expected low surrogate")
          else do
            let !w = unsafeShiftL (fromIntegral (c - 0xd800)) 10 + fromIntegral (c2 - 0xdc00) + 0x0010000 :: Word32
            write8 mba boff     $ 0xf0 + unsafeShiftR w 18
            write8 mba (boff+1) $ 0x80 + (unsafeShiftR w 12 .&. 0x3f)
            write8 mba (boff+2) $ 0x80 + (unsafeShiftR w 6 .&. 0x3f)
            write8 mba (boff+3) $ 0x80 + (w .&. 0x3f)
            go mba tba (toff+2) (tlen-2) (boff+4)
      | c <= 0xdfff = return $ Just (EncodingErorr boff "unexpected low surrogate")
      | otherwise = do
        write8 mba boff     $ 0xe0 + unsafeShiftR c 12
        write8 mba (boff+1) $ 0x80 + (unsafeShiftR c 6 .&. 0x3f)
        write8 mba (boff+2) $ 0x80 + (c .&. 0x3f)
        go mba tba (toff+1) (tlen-1) (boff+3)
      where !c = indexByteArray tba toff :: Word16

instance IsString Line where
  fromString xs = case encodeLine (Text.pack xs) of
    Left e -> error (show e)
    Right l -> l

instance IsList Line where
  type Item Line = Word8
  fromList xs = fromListN (Prelude.length xs) xs
  fromListN n xs0 = Line $ runST $ do
      mba <- newByteArray n
      go mba 0 xs0
    where
      go :: MutableByteArray s -> Int -> [Word8] -> ST s ByteArray
      go !mba !i (x:xs) = writeByteArray mba i x >> go mba (i + 1) xs
      go mba _ [] = unsafeFreezeByteArray mba
  toList l = (l!!) <$> [0 .. size l - 1]

instance AsEmpty Line where
  _Empty = prism (const emptyLine) $ \l -> if size l == 0
    then Right ()
    else Left l

emptyLine :: Line
emptyLine = Line $ runST $ newPinnedByteArray 0 >>= unsafeFreezeByteArray
{-# NOINLINE emptyLine #-}

--------------------------------------------------------------------------------
-- HasLine
--------------------------------------------------------------------------------

class HasLine t where
  line :: t -> Line

instance HasDelta Line where
  delta = sizeofByteArray ba

-- |
-- Change in UTF-16 code unit count from a UTF-8 byte
--
-- @
-- 0xxxxxxx -- bump by 1
-- 10xxxxxx -- leave, this is a tail-byte
-- 11xxxxxx -- bump by 1
-- 1111xxxx -- bump by 2
-- @
bump16 :: Word8 -> Int
bump16 w
  | w <= 127  = 1
  | w <= 191  = 0
  | w <= 223  = 1
  | otherwise = 2

-- | Convert from UTF-8 bytes to UTF-16 code-units
bytesTocodeUnits :: HasLine l => l -> Int -> Int
bytesTocodeUnits l0 p0 = go (line l) 0 p where
  go !_ !acc !0 = acc
  go l acc p    = go l (acc + bump16 (l !! p)) (p-1)

-- | Convert count from UTF-16 code-untis to UTF-8 bytes
--
-- /O(b)/ in the column position.
-- Could become /O(1)/ with a rank structure
codeUnitsToBytes :: HasLine l => l -> Int -> Int
codeUnitsToBytes l0 c0 = go (line l0) 0 c where
  go !l !p 0 = go2 l p 
  go l p c 
    | p < delta l = go l (p+1) (c - bump16 (l !! p))
    | otherwise = delta l
  go2 !l !p -- skip tail bytes
    | p < delta l, b <- l!!p, 128 <= b, b <= 191 = go2 l (p+1)
    | otherwise = p

--------------------------------------------------------------------------------
-- LineMeasure
--------------------------------------------------------------------------------

data LineMeasure = LineMeasure !Int !Int
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance Hashable LineMeasure

instance Semigroup LineMeasure where
  LineMeasure l b <> LineMeasure l' b' = LineMeasure (l + l') (b + b')

instance Monoid LineMeasure where
  mempty = LineMeasure 0 0

--------------------------------------------------------------------------------
-- HasLineMeasure
--------------------------------------------------------------------------------

class HasDelta t => HasLineMeasure t where
  lineCount :: t -> LineMeasure

instance HasDelta LineMeasure where
  delta (LineMeasure l d) = d

instance HasLineMeasure LineMeasure where
  lineCount (LineMeasure l d) = l

--------------------------------------------------------------------------------
-- Position <-> Delta
--------------------------------------------------------------------------------

-- | Compute an Language Server Protocol 'Position' from a 'Delta' given the associated text
--
-- Takes /O(log l + c)/ where l is the number of lines and c is column of the position.
--
-- We could reduce this to /O(log l)/ by storing a succinct-style select structure with the
-- lines or /O(log l + log c)/ by storing just a rank structure.
deltaToPosition :: (Measured v a, HasLineMeasure v, HasLine a) => FingerTree v a -> Delta -> Position
deltaToPosition t d = case split (\x -> delta x >= d) t of
    (l, r) | ml <- measure l -> case viewR r of
       EmptyR -> Position (lineCount ml) 0
       m :< _ -> Position (lineCount ml) $ byteToCodeUnits m (d - delta ml) 


-- | Convert from a Language Server Protocol 'Position' to a 'Delta' given the associated text.
--
-- /O(log l + c)/
positionToDelta :: (Measured v a, HasLineMeasure v, HasLine a) => FingerTree v a -> Position -> Delta
positionToDelta t (Position nl c16) = case split (\x -> lineCount x >= nl) t of
    (l, r) | ml <- measure l -> case viewR r of
      EmptyR -> delta ml
      m :< _ -> delta ml + codeUnitsToDelta m c

