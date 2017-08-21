{-# language CPP #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# options_ghc -Wall -Wno-unused-matches #-}

#ifdef HLINT
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

module Main (main) where

import Control.Monad (when)
import Data.List (unwords)
import Distribution.Extra.Doctest (generateBuildModule)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (PackageDescription, package)
import Distribution.Simple (defaultMainWithHooks, buildHook, cleanHook, instHook, copyHook, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), withExeLBI)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import Distribution.Simple.Setup (BuildFlags(buildVerbosity), fromFlag)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (FilePath, (</>), (<.>))
import System.Process (rawSystem)

#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
#else
import Distribution.Simple.BuildPaths (autogenModulesDir)
#endif

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.Executable (exeName)
#else
import Distribution.PackageDescription (exeName)
#endif

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
    generateBuildModule "doctests" flags pkg lbi
    generateBuildModule "hlint" flags pkg lbi
    generateCodaBuildModule flags pkg lbi
    buildHook simpleUserHooks pkg lbi hooks flags
    run (buildDir lbi </> "build-coda" </> "build-coda" <.> exe) ["all"]
  , cleanHook = \pkg lbi hooks flags -> do
    cleanHook simpleUserHooks pkg lbi hooks flags
    doesDirectoryExist "bin" >>= \x -> when x $ removeDirectoryRecursive "bin"
    doesDirectoryExist "node_modules" >>= \x -> when x $ removeDirectoryRecursive "node_modules"
  , instHook = \pkg lbi hooks flags -> do
    instHook simpleUserHooks pkg lbi hooks flags
    run (buildDir lbi </> "build-coda" </> "build-coda" <.> exe) ["install"]
  , copyHook = \pkg lbi hooks flags -> do
    copyHook simpleUserHooks pkg lbi hooks flags
    run (buildDir lbi </> "build-coda" </> "build-coda" <.> exe) ["copy"]
  }

run :: FilePath -> [String] -> IO ()
run cmd args = rawSystem cmd args >>= \case
  ExitSuccess -> return ()
  ExitFailure e -> do
    putStrLn $ "error: " ++ unwords (cmd:args) ++ " exited with status code " ++ show e
    exitWith $ ExitFailure e

generateCodaBuildModule :: BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generateCodaBuildModule flags pkg lbi = do
  let verbosity = fromFlag (buildVerbosity flags)
  withExeLBI pkg lbi $ \executable clbi -> when (exeName executable == "build-coda") $ do
#if MIN_VERSION_Cabal(1,25,0)
    let testAutogenDir = autogenComponentModulesDir lbi clbi
#else
    let testAutogenDir = autogenModulesDir lbi
#endif
    createDirectoryIfMissingVerbose verbosity True testAutogenDir
    rewriteFile (testAutogenDir </> "Build_coda.hs") $ unlines
      [ "{-# options_ghc -Wno-deprecations #-}"
      , "module Build_coda where"
      , ""
      , "import Distribution.Version"
      , ""
      , "buildDir :: FilePath"
      , "buildDir = " ++ show (buildDir lbi)
      , ""
      , "packageVersion :: Version"
      , "packageVersion = " ++ show (pkgVersion $ package pkg)
      ]

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

-- | The extension of executables, @\"exe\"@ on Windows and @\"\"@ otherwise.
exe :: FilePath
exe = if isWindows then "exe" else ""
