{-# options_ghc -Wall #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import qualified Distribution.Extra.Doctest as Doctest
import qualified Distribution.Simple as Cabal

main :: IO ()
main = Cabal.defaultMainWithHooks Cabal.simpleUserHooks
  { Cabal.buildHook = \pkg lbi hooks flags -> do
      Doctest.generateBuildModule "doctest" flags pkg lbi
      Doctest.generateBuildModule "hlint" flags pkg lbi
      Cabal.buildHook Cabal.simpleUserHooks pkg lbi hooks flags
  }
