{-# language BangPatterns #-}

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
import qualified Data.ByteString as B
import Data.Traversable (for)
import Data.List (unwords)
import Data.Maybe (catMaybes)
import Test.DocTest

data Acc = Acc !Int !Int

-- | Quickly filter for files that actually contain '>>>'
possible :: [FilePath] -> IO [FilePath]
possible xs = do
    fmap catMaybes $ for xs $ \x -> do
      !content <- B.readFile x
      return $! if testy (B.foldl' ticks (Acc 0 0) content) then Just x else Nothing
  where
    ticks (Acc a b) 62 = Acc (a+1) b -- 62 = fromEnum '>'
    ticks (Acc a b) _  = Acc 0 (max a b)
    testy (Acc _ b) = b >= 3

main :: IO ()
main = do
  tests <- possible files
  let args = "-itest/shim" : flags ++ pkgs ++ tests
  putStrLn $ unwords ("doctest":args)
  doctest args
