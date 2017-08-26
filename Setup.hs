{-# language CPP, LambdaCase #-}
{-# options_ghc -Wall -threaded -rtsopts -with-rtsopts=-I0 -with-rtsopts=-qg -with-rtsopts=-qg #-}
#ifdef HLINT
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

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

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.List (intercalate)
import Development.Shake as Shake
import Development.Shake.FilePath
import Distribution.Extra.Doctest (generateBuildModule)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (PackageDescription, package)
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.Find (findProgramOnSearchPath, defaultProgramSearchPath)
import Distribution.Simple.Setup (buildVerbosity, regVerbosity, copyVerbosity, testVerbosity, fromFlag, Flag(..))
import qualified Distribution.Verbosity as Cabal
import Distribution.Version (Version(..))
import System.Directory (makeAbsolute, copyFile, createDirectoryIfMissing, renameFile)
import System.Environment (lookupEnv, withArgs)

reverb :: Cabal.Verbosity -> Shake.Verbosity
reverb v
  | v == Cabal.silent  = Shake.Silent
                      -- Shake.Quiet
  | v == Cabal.normal  = Shake.Normal
  | v == Cabal.verbose = Shake.Loud
                      -- Shake.Chatty
  | otherwise          = Shake.Diagnostic

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = buildHook'
  , regHook = regHook'
  , testHook = testHook'
  , copyHook = copyHook'
  } where

  buildHook' pkg lbi hooks flags = build pkg lbi (buildVerbosity flags) "build" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-build" $ do
      withResource cabal 1 $ do
        putLoud "Building with cabal"
        liftIO $ do
          -- don't do these first two in parallel as they may clobber the same file!
          generateBuildModule "coda-doctests" flags pkg lbi
          generateBuildModule "coda-hlint" flags pkg lbi
          buildHook simpleUserHooks pkg lbi hooks flags

  regHook' pkg lbi hooks flags = build pkg lbi (regVerbosity flags) "register" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-build" $ putLoud "Registering existing build"
    phony "cabal-register" $ do
      putLoud "Registering with cabal"
      withResource cabal 1 $ liftIO $ regHook simpleUserHooks pkg lbi hooks flags

  copyHook' pkg lbi hooks flags = build pkg lbi (copyVerbosity flags) "copy" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-build" $ putLoud "Registering existing build"
    phony "cabal-copy" $ do
      putLoud "Copying with cabal"
      withResource cabal 1 $ liftIO $ copyHook simpleUserHooks pkg lbi hooks flags

  testHook' args pkg lbi hooks flags = build pkg lbi (testVerbosity flags) "test" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-test" $ do
      putLoud "Testing with cabal"
      withResource cabal 1 $ liftIO $ testHook simpleUserHooks args pkg lbi hooks flags

build :: PackageDescription -> LocalBuildInfo -> Flag Cabal.Verbosity -> String -> Rules () -> IO ()
build pkg lbi verb xs extraRules = do
  let ver = intercalate "." [ show x | x <- tail $ versionNumbers $ pkgVersion (package pkg) ]
      vsix = buildDir lbi </> ("coda-" ++ ver) <.> "vsix"
      extDir = buildDir lbi </> "ext"
      coda_exe = buildDir lbi </> "coda" </> "coda" <.> exe
      progress p = lookupEnv "CI" >>= \case
        Just "true" -> return ()
        _ -> do
          program <- progressProgram
          progressDisplay 0.5 (\s -> progressTitlebar s >> program s) p

  extensionFiles <- filter (/= "package-lock.json") <$> getDirectoryFilesIO "ext" ["//*"]
  markdownFiles <- getDirectoryFilesIO "" ["*.md"]
  has_cached_vscode_d_ts <- not . null <$> getDirectoryFilesIO "var" ["vscode.d.ts"]
  has_cached_package_lock <- not . null <$> getDirectoryFilesIO "var" ["package-lock.json"]
  let node_modules = buildDir lbi </> "ext_node_modules_installed"
  let vscode_d_ts = extDir </> "node_modules/vscode/vscode.d.ts"
  let package_lock = extDir </> "package-lock.json"
  let extDirExtensionFiles = map (extDir </>) extensionFiles


  withArgs [xs] $ shakeArgs shakeOptions
      { shakeFiles = buildDir lbi
      , shakeThreads = 0
      , shakeProgress = progress
      , shakeLineBuffering = False
      , shakeVerbosity = reverb (fromFlag verb)
      } $ do
    action $ putLoud $ "Running " ++ xs

    extraRules

    npmResource <- newResource "npm" 1
    let npm :: [CmdOption] -> [String] -> Action ()
        npm opts args = withResource npmResource 1 $ command_ (Cwd extDir : Shell : opts)  "npm" args

    phony "build" $ need ["cabal-build", vsix]

    phony "copy" $ do
      need ["cabal-copy", vsix]
      liftIO (findProgramOnSearchPath Cabal.verbose defaultProgramSearchPath "code") >>= \case
        Just (code, _) -> do
          command_ [] code ["--install-extension", vsix]
          putNormal "Installed into Visual Studio Code"
        Nothing -> do
          absVsix <- liftIO (makeAbsolute vsix)
          putNormal "Unable to install: 'code' not found"
          putNormal $ "Package: " ++ absVsix

    phony "register" $ need ["cabal-register"]

    phony "test" $ do
      need $ "cabal-test" : extDirExtensionFiles
      npm [] ["run-script","lint"]
      -- npm [] ["run-script","test"] -- download vscode and run the ext/test suite

    vsix %> \_ -> do
      need $ [extDir </> "extension.js", extDir </> "bin/coda" <.> exe, node_modules]
          ++ extDirExtensionFiles
          ++ map (extDir </>) markdownFiles
      command_ [Shell, Cwd extDir] ("." </> "node_modules" </> ".bin" </> "vsce") ["package","-o","coda.vsix"]
      liftIO $ renameFile (extDir </> "coda.vsix") vsix
      putNormal $ "Packaged extension: " ++ vsix

    node_modules %> \out -> do
      need extDirExtensionFiles
      when has_cached_package_lock $ do
        putLoud "Using cached package-lock.json"
        liftIO $ copyFile "var/package-lock.json" package_lock -- untracked
      npm [] ["install","--ignore-scripts"]
      unless has_cached_package_lock $ do
        putLoud "Caching package-lock.json"
        liftIO $ do
          createDirectoryIfMissing False "var"
          copyFile package_lock "var/package-lock.json" -- untracked
      writeFile' out "" -- touch an indicator file

    -- Download an appropriate vscode.d.ts from github
    vscode_d_ts %> \out -> if has_cached_vscode_d_ts
      then do
        need [node_modules] -- need a place to put it
        putLoud "Using cached vscode.d.ts"
        copyFile' "var/vscode.d.ts" vscode_d_ts
      else do
        need [node_modules]
        putLoud "Downloading vscode.d.ts"
        quietly $ npm [] ["run-script","update-vscode"]
        liftIO $ do
          createDirectoryIfMissing False "var"
          copyFile out "var/vscode.d.ts" -- untracked

    extDir </> "bin/coda" <.> exe %> \_ -> do
      need ["cabal-build"]
      -- Assumes coda executable was built by cabal as forced by the build-tool annotations in the .cabal file.
      copyFileChanged coda_exe (extDir </> "bin/coda" <.> exe)

    extDir </> "extension.js" %> \_ -> do
      need (vscode_d_ts : extDirExtensionFiles)
      npm [WithStdout True, EchoStdout False] ["run-script", "compile"]

    for_ extensionFiles $ \file -> extDir </> file %> \out -> copyFile' ("ext" </> file) out
    for_ markdownFiles $ \file -> extDir </> file %> \out -> copyFile' file out

#if !MIN_VERSION_Cabal(2,0,0)
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif
