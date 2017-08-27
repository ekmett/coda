{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

module Coda.Util.STM 
  ( MonadSTM 
  , liftSTM
  ) where

import Control.Concurrent.STM
import Control.Monad.Base

class MonadBase STM m => MonadSTM m
instance MonadBase STM m => MonadSTM m

liftSTM :: MonadSTM t => STM a -> t a
liftSTM = liftBase 
