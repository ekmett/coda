{-# language CPP #-}

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

module Coda.Version
  ( version
  ) where

import Data.List (intercalate)
import Data.Version
import qualified Paths_coda

-- | Grab the version number from this project.
version :: String
version = intercalate "." $ show <$> tail (versionBranch Paths_coda.version)
