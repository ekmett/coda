---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Util.Async 
  ( waitAll
  , waitAllSTM
  , waitAll_
  , waitAllSTM_
  ) where 

import Control.Concurrent.Async
import Control.Concurrent.STM

-- | Wait for a bunch of 'Async' computations in the 'STM' monad
waitAllSTM :: [Async a] -> STM [a]
waitAllSTM [] = pure []
waitAllSTM (x:xs) = (:) <$> (waitSTM x `orElse` (waitAllSTM xs *> retry)) <*> waitAllSTM xs

-- | Wait for a bunch of 'Async' computations
waitAll :: [Async a] -> IO [a]
waitAll = atomically . waitAllSTM

-- | Wait for a bunch of 'Async' computations in the 'STM' monad, discarding the results.
waitAllSTM_ :: [Async a] -> STM ()
waitAllSTM_ [] = pure ()
waitAllSTM_ (x:xs) = (waitSTM x `orElse` (waitAllSTM_ xs *> retry)) *> waitAllSTM_ xs

-- | Wait for a bunch of 'Async' computations, discarding the results.
waitAll_ :: [Async a] -> IO ()
waitAll_ = atomically . waitAllSTM_
