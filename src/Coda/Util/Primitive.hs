{-# language MagicHash #-}
{-# language UnboxedTuples #-}
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
