--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--
-- This module runs HLint on the local source tree.
-----------------------------------------------------------------------------
module Main where

import Build (files)
import Data.Foldable (traverse_)
import Data.List (unwords)
import Language.Haskell.HLint

main :: IO ()
main = do
    let args = ["lint"] ++ files ++ ["--cpp-define=HLINT", "--cpp-ansi"]
    putStrLn $ unwords ("hlint":args)
    hints <- hlint args
    traverse_ print hints
    -- unless (null hints) exitFailure
