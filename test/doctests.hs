-----------------------------------------------------------------------------
-- |
-- Module      :  Main (doctests)
-- Copyright   :  (C) 2012-17 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides doctests for a project based on the actual versions
-- of the packages it was built with. It requires a corresponding Setup.lhs
-- to be added to the project
-----------------------------------------------------------------------------
module Main where

import Build_doctests
import Data.List (unwords)
import Test.DocTest

toFile :: String -> String
toFile xs = "src/" ++ map (\c -> if c == '.' then '/' else c) xs ++ ".hs"

main :: IO ()
main = do
  let args = flags ++ pkgs ++ map toFile module_sources
  putStrLn $ unwords ("doctest":args)
  doctest args
