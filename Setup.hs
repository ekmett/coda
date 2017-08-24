{-# language LambdaCase, MultiWayIf #-}
{-# options_ghc -Wall -threaded -rtsopts -with-rtsopts=-I0 -with-rtsopts=-qg -with-rtsopts=-qg #-}

module Main where

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.List (intercalate)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Extra.Doctest (generateBuildModule)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (PackageDescription, package)
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Version (Version(..))
import System.Directory (makeAbsolute, copyFile, createDirectoryIfMissing, renameFile)
import System.Environment (lookupEnv, withArgs)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { buildHook = buildHook'
  , regHook = regHook'
  , testHook = testHook'
  } where
  buildHook' pkg lbi hooks flags = build pkg lbi "build" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-build" $ do
      withResource cabal 1 $ do
        putLoud "Building with cabal"
        liftIO $ do
          -- don't do these first two in parallel as they may clobber the same file!
          generateBuildModule "coda-doctests" flags pkg lbi
          generateBuildModule "coda-hlint" flags pkg lbi
          buildHook simpleUserHooks pkg lbi hooks flags
  regHook' pkg lbi hooks flags = build pkg lbi "register" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-build" $ putLoud "Registering existing build"
    phony "cabal-register" $ do
      putLoud "Registering with cabal"
      withResource cabal 1 $ liftIO $ regHook simpleUserHooks pkg lbi hooks flags
  testHook' args pkg lbi hooks flags = build pkg lbi "test" $ do
    cabal <- newResource "cabal" 1
    phony "cabal-test" $ do
      putLoud "Testing with cabal"
      withResource cabal 1 $ liftIO $ testHook simpleUserHooks args pkg lbi hooks flags

build :: PackageDescription -> LocalBuildInfo -> String -> Rules () -> IO ()
build pkg lbi xs extraRules = do
  putStrLn $ "Running " ++ xs
  let ver = intercalate "." [ show x | x <- tail $ versionBranch $ pkgVersion (package pkg) ]
      vsix = buildDir lbi </> ("coda-" ++ ver) <.> "vsix"
      extDir = buildDir lbi </> "extension"
      coda_exe = buildDir lbi </> "coda" </> "coda" <.> exe
      progress p = lookupEnv "CI" >>= \case
        Just "true" -> return ()
        _ -> do
          program <- progressProgram
          progressDisplay 0.5 (\s -> progressTitlebar s >> program s) p

  absolutePackage <- makeAbsolute vsix
  absoluteVscePath <- makeAbsolute $ extDir </> "node_modules/vsce/out"
  extensionFiles <- filter (/= "package-lock.json") <$> getDirectoryFilesIO "ext" ["//*"]
  markdownFiles <- getDirectoryFilesIO "" ["*.md"]
  has_cached_vscode_d_ts <- not . null <$> getDirectoryFilesIO "var" ["vscode.d.ts"]
  has_cached_package_lock <- not . null <$> getDirectoryFilesIO "var" ["package-lock.json"]
  let node_modules = buildDir lbi </> "extension_node_modules_installed"
  let vscode_d_ts = extDir </> "node_modules/vscode/vscode.d.ts"
  let package_lock = extDir </> "package-lock.json"
  let extDirExtensionFiles = map (extDir </>) extensionFiles

  withArgs [xs] $ shakeArgs shakeOptions
      { shakeFiles = buildDir lbi
      , shakeThreads = 0
      , shakeProgress = progress
      , shakeLineBuffering = False
      , shakeVerbosity = Loud -- TODO: adapt based on build flags?
      } $ do

    extraRules

    npmResource <- newResource "npm" 1
    let npm :: [String] -> Action ()
        npm args = withResource npmResource 1 $ command_ [Cwd extDir, Shell] "npm" args

    phony "build" $
      need ["cabal-build", vsix]

    phony "register" $ do
      need ["cabal-register",vsix]
      cmd Shell "code" "--install-extension" [vsix]

    phony "test" $ do
      need $ "cabal-test" : extDirExtensionFiles
      npm ["run-script","lint"]

    vsix %> \_ -> do
      need $ [extDir </> "bin/extension.js", extDir </> "bin/coda" <.> exe, node_modules]
          ++ extDirExtensionFiles
          ++ map (extDir </>) markdownFiles
      cmd Shell (Cwd extDir) ("node_modules" </> "vsce" </> "out" </> "vsce") "package" "-o" "coda.vsix"
      liftIO $ renameFile (extDir </> "coda.vsix") package

    node_modules %> \out -> do
      need extDirExtensionFiles
      when has_cached_package_lock $ do
        putLoud "Using cached package-lock.json"
        liftIO $ copyFile "var/package-lock.json" package_lock -- untracked
      npm ["install","--ignore-scripts"]
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
        npm ["run-script","update-vscode"]
        liftIO $ do
          createDirectoryIfMissing False "var"
          copyFile out "var/vscode.d.ts" -- untracked

    extDir </> "bin/coda" <.> exe %> \_ -> do
      need ["cabal-build"]
      -- Assumes coda executable was built by cabal as forced by the build-tool annotations in the .cabal file.
      copyFileChanged coda_exe (extDir </> "bin/coda" <.> exe)

    extDir </> "bin/extension.js" %> \_ -> do
      need (vscode_d_ts : extDirExtensionFiles)
      npm ["run-script","compile"]

    for_ extensionFiles $ \file -> extDir </> file %> \out -> copyFile' ("ext" </> file) out
    for_ markdownFiles $ \file -> extDir </> file %> \out -> copyFile' file out
