{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Util.STM 
  ( MonadSTM 
  , liftSTM
  ) where

import Control.Concurrent.STM
import Control.Monad.Base

class MonadBase STM m => MonadSTM m
instance MonadBase STM m => MonadSTM m

-- |
-- @
-- 'liftSTM' = 'liftBase'
-- @
liftSTM :: MonadSTM t => STM a -> t a
liftSTM = liftBase 
