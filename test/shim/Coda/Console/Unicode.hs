--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This stub prevents doctest from flipping out over an failing to
-- find the pre-processed @src/Coda/Console/Unicode.hsc@ file.
--------------------------------------------------------------------

module Coda.Console.Unicode
  ( withUnicode
  ) where

import Control.Monad.Catch

withUnicode :: MonadCatch m => m a -> m a
withUnicode m = m
