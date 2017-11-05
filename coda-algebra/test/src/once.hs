
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD2 (see the file LICENSE.md)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Main where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Map as Map

test :: Map Delta Delta
test = Map.union (Map.insert 0 0 (Map.insert 1 0 mempty)) (rel 1 (Map.insert 1 0 (Map.insert 0 0 mempty))) 

main :: IO ()
main = do
  print test
  return ()
