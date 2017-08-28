{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}

module Coda.Util.Primitive 
  ( shrinkMutableByteArray
  ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import GHC.Int
import GHC.Prim (shrinkMutableByteArray#)

shrinkMutableByteArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m ()
shrinkMutableByteArray (MutableByteArray mba#) (I# n) = 
   primitive_ $ \s -> shrinkMutableByteArray# mba# n s
{-# INLINE shrinkMutableByteArray #-}
