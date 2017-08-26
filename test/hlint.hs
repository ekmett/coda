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

import Build_doctests (module_sources)
import Data.Foldable (traverse_)
import Data.List (unwords)
import Language.Haskell.HLint
-- import Control.Monad (unless)
-- import System.Exit

toFile :: String -> String
toFile xs = "src/" ++ map (\c -> if c == '.' then '/' else c) xs ++ ".hs"

main :: IO ()
main = do
    let args = ["lint"] ++ map toFile module_sources ++ ["--cpp-define=HLINT", "--cpp-ansi"]
    putStrLn $ unwords ("hlint":args)
    hints <- hlint args
    traverse_ print hints
    -- unless (null hints) exitFailure
