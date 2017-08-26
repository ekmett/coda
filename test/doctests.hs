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

import Build_doctests
import Data.List (notElem, unwords)
import Test.DocTest

exceptions :: [String]
exceptions =
  [ "Coda.Console.Unicode"
  , "Paths_coda"
  ]

toFile :: String -> String
toFile xs = "src/" ++ map (\c -> if c == '.' then '/' else c) xs ++ ".hs"

main :: IO ()
main = do
  let args = "-itest/doctests" : flags ++ pkgs ++ map toFile (filter (`notElem` exceptions) module_sources)
  putStrLn $ unwords ("doctest":args)
  doctest args
