{-# language CPP #-}

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

module Coda.Version
  ( version
  , versionNumbers
  ) where

import Data.List (intercalate)
import Distribution.Version
import qualified Paths_coda

-- | Grab the version number from this project.
version :: String
version = intercalate "." $ show <$> tail (versionNumbers Paths_coda.version)

#if !MIN_VERSION_Cabal(2,0,0)
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif
