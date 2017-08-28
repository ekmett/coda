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

waitAllSTM :: [Async a] -> STM [a]
waitAllSTM [] = pure []
waitAllSTM (x:xs) = (:) <$> (waitSTM x `orElse` (waitAllSTM xs *> retry)) <*> waitAllSTM xs

waitAll :: [Async a] -> IO [a]
waitAll = atomically . waitAllSTM

waitAllSTM_ :: [Async a] -> STM ()
waitAllSTM_ [] = pure ()
waitAllSTM_ (x:xs) = (waitSTM x `orElse` (waitAllSTM_ xs *> retry)) *> waitAllSTM_ xs

waitAll_ :: [Async a] -> IO ()
waitAll_ = atomically . waitAllSTM_
