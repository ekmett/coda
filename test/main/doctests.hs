--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2012-2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides doctests for a project based on the actual versions
-- of the packages it was built with. It requires a corresponding Setup.lhs
-- to be added to the project
--------------------------------------------------------------------------------

module Main where

import Build (files)
import Build_doctests (flags, pkgs)
import Data.List (unwords)
import Test.DocTest

main :: IO ()
main = do
  let args = "-itest/shim" : flags ++ pkgs ++ files
  putStrLn $ unwords ("doctest":args)
  doctest args
