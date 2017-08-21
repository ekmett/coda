-----------------------------------------------------------------------------
-- |
-- Module      :  Main (hlint)
-- Copyright   :  (C) 2013-2017 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module runs HLint on the local source tree.
-----------------------------------------------------------------------------
module Main where

import Control.Monad
import Data.Foldable (traverse_)
import Language.Haskell.HLint
import System.Environment
import System.Exit
import Build_doctests

main :: IO ()
main = do
    args <- getArgs
    hints <- hlint $ ["lint"] ++ module_sources ++ ["--cpp-define=HLINT", "--cpp-ansi"]
    traverse_ (putStrLn . show) hints
    -- unless (null hints) exitFailure
