{-# language CPP, LambdaCase, StandaloneDeriving, ScopedTypeVariables #-}
{-# options_ghc -Wall -Wno-orphans -threaded -rtsopts -with-rtsopts=-I0 -with-rtsopts=-qg -with-rtsopts=-qg #-}
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
import Data.Aeson
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable as Foldable (for_, foldl')
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set as Set
import Development.Shake as Shake
import Development.Shake.FilePath as Shake
import Distribution.Extra.Doctest as Doctest
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import qualified Distribution.License as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal hiding (Flag)
import qualified Distribution.Simple as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.PackageIndex as Cabal
import qualified Distribution.Simple.Program.Find as Cabal
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Verbosity as Cabal
import Distribution.Text as Cabal
import Distribution.Version
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
main = Cabal.defaultMainWithHooks Cabal.simpleUserHooks
  { Cabal.buildHook = buildHook'
  , Cabal.regHook = regHook'
  , Cabal.testHook = testHook'
  , Cabal.copyHook = copyHook'
  } where

  buildHook' pkg lbi hooks flags = build pkg lbi (Cabal.buildVerbosity flags) "build" $ do
    cabal <- newResource "cabal" 1
    "cabal-build" ~> do
      putLoud "Building with cabal"
      withResource cabal 1 $ liftIO $ do
        -- don't do these first two in parallel as they may clobber the same file!
        Doctest.generateBuildModule "doctest" flags pkg lbi
        Doctest.generateBuildModule "hlint" flags pkg lbi
        Cabal.buildHook Cabal.simpleUserHooks pkg lbi hooks flags

  regHook' pkg lbi hooks flags = build pkg lbi (Cabal.regVerbosity flags) "register" $ do
    cabal <- newResource "cabal" 1
    "cabal-build" ~> pure ()
    "cabal-register" ~> do
      putLoud "Registering with cabal"
      withResource cabal 1 $ liftIO $ Cabal.regHook Cabal.simpleUserHooks pkg lbi hooks flags

  copyHook' pkg lbi hooks flags = build pkg lbi (Cabal.copyVerbosity flags) "copy" $ do
    cabal <- newResource "cabal" 1
    "cabal-build" ~> pure ()
    "cabal-copy" ~> do
      putLoud "Copying with cabal"
      withResource cabal 1 $ liftIO $ Cabal.copyHook Cabal.simpleUserHooks pkg lbi hooks flags

  testHook' args pkg lbi hooks flags = build pkg lbi (Cabal.testVerbosity flags) "test" $ do
    cabal <- newResource "cabal" 1
    "cabal-test" ~> do
      putLoud "Testing with cabal"
      withResource cabal 1 $ liftIO $ Cabal.testHook Cabal.simpleUserHooks args pkg lbi hooks flags

build :: Cabal.PackageDescription -> Cabal.LocalBuildInfo -> Cabal.Flag Cabal.Verbosity -> String -> Shake.Rules () -> IO ()
build pkg lbi verb xs extraRules = do
  let ver = intercalate "." [ show x | x <- tail $ versionNumbers $ Cabal.pkgVersion (Cabal.package pkg) ]
      vsix = Cabal.buildDir lbi </> ("coda-" ++ ver) <.> "vsix"
      extDir = Cabal.buildDir lbi </> "ext"
      coda_exe = Cabal.buildDir lbi </> "coda" </> "coda" <.> exe
      progress p = lookupEnv "CI" >>= \case
        Just "true" -> return ()
        _ -> do
          program <- progressProgram
          progressDisplay 0.5 (\s -> progressTitlebar s >> program s) p

  extensionFiles <- Prelude.filter (/= "package-lock.json") <$> getDirectoryFilesIO "ext" ["//*"]
  markdownFiles <- Prelude.filter (/= "LICENSE.md") <$> getDirectoryFilesIO "" ["*.md"]
  has_cached_vscode_d_ts  <- not . Prelude.null <$> getDirectoryFilesIO "var" ["vscode.d.ts"]
  has_cached_package_lock <- not . Prelude.null <$> getDirectoryFilesIO "var" ["package-lock.json"]
  let node_modules = Cabal.buildDir lbi </> "ext_node_modules_installed"
  let vscode_d_ts = extDir </> "node_modules/vscode/vscode.d.ts"
  let package_lock = extDir </> "package-lock.json"
  let extDirExtensionFiles = (extDir </>) <$> extensionFiles
  let licenses = groupByLicense (dependencyInstalledPackageInfos lbi)

  withArgs [xs] $ shakeArgs shakeOptions
      { shakeFiles = Cabal.buildDir lbi
      , shakeThreads = 0
      , shakeProgress = progress
      , shakeLineBuffering = False
      , shakeVerbosity = reverb (Cabal.fromFlag verb)
      } $ do
    action $ putLoud $ "Running " ++ xs

    extraRules

    npmResource <- newResource "npm" 1
    let npm opts args = withResource npmResource 1 $
          command_ (Traced (unwords ("npm":args)) : Cwd extDir : Shell : opts)  "npm" args

    "build" ~> do
       need ["cabal-build", vsix]

    "copy" ~> do
      need ["cabal-copy", vsix]
      liftIO (Cabal.findProgramOnSearchPath Cabal.verbose Cabal.defaultProgramSearchPath "code") >>= \case
        Just (code, _) -> do
          command_ [] code ["--install-extension", vsix]
          putNormal "Installed into Visual Studio Code"
        Nothing -> do
          absVsix <- liftIO (makeAbsolute vsix)
          putNormal "Unable to install: 'code' not found"
          putNormal $ "Package: " ++ absVsix

    "register" ~> need ["cabal-register"]

    "test" ~> do
      need $ "cabal-test" : extDirExtensionFiles
      npm [] ["run-script","lint"]
      -- npm [] ["run-script","test"] -- download vscode and run the ext/test suite

    vsix %> \_ -> do
      need $ [extDir </> "extension.js", extDir </> "bin/coda" <.> exe, node_modules, extDir </> "LICENSE.md"]
          ++ extDirExtensionFiles
          ++ fmap (extDir </>) markdownFiles
      withResource npmResource 1 $ command_
        [WithStdout True, EchoStdout False, Shell, Cwd extDir]
        ("." </> "node_modules" </> ".bin" </> "vsce")
        ["package","-o","coda.vsix"]
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

    extDir </> "LICENSE.md" %> \out -> do
      alwaysRerun
      mainLicense <- readFile' "LICENSE.md"
      let unexpectedLicenses = Set.difference (Map.keysSet licenses) acceptableLicenses
      unless (Prelude.null unexpectedLicenses) $
        for_ unexpectedLicenses $ \license ->
          for_ (Map.lookup license licenses) $ \ deps ->
             for_ deps $ \dep ->
               putQuiet $ "Warning: dependency '" ++ show dep ++ " has an unexpected license: "  ++ Cabal.display license
      need [node_modules]

      Stdout metadata <- withResource npmResource 1 $ command
        [Shell, Cwd extDir]
        ("." </> "node_modules" </> ".bin" </> "license-checker")
        ["--production","--json","--relativeLicensePath"]

      writeFile' out
         $ "<!-- Auto generated by 'cabal build'. Do not edit by hand. -->\n\n"
        ++ mainLicense
        ++ "\n\n# Third-Party Licenses\n\n"
        ++ "This project was built with third-party dependencies with the following licenses\n"
        ++ "\n\n## Haskell\n\n"
        ++ haskellLicenseList licenses
        ++ "\n\n## Node\n\n"
        ++ nodeLicenseList metadata

findTransitiveDependencies :: Cabal.PackageIndex InstalledPackageInfo.InstalledPackageInfo -> Set Cabal.UnitId -> Set Cabal.UnitId
findTransitiveDependencies pkgIdx set0 = go Set.empty (Set.toList set0) where
  go set []  = set
  go set (q : queue)
    | q `Set.member` set = go set queue
    | otherwise = case Cabal.lookupUnitId pkgIdx q of
      Nothing  -> go set queue
      Just ipi -> go (Set.insert q set) (InstalledPackageInfo.depends ipi ++ queue)

dependencyUnitIds :: Cabal.LocalBuildInfo -> Set Cabal.UnitId
dependencyUnitIds lbi
  = findTransitiveDependencies (Cabal.installedPkgs lbi) $ Set.fromList $ do
    (_, clbi, _) <- Cabal.componentsConfigs lbi
    fst <$> Cabal.componentPackageDeps clbi

dependencyInstalledPackageInfos :: Cabal.LocalBuildInfo -> [InstalledPackageInfo]
dependencyInstalledPackageInfos lbi
  = catMaybes
  $ Cabal.lookupUnitId (Cabal.installedPkgs lbi) <$> Set.toList (dependencyUnitIds lbi)

deriving instance Ord Cabal.License

groupByLicense :: [InstalledPackageInfo] -> Map Cabal.License [InstalledPackageInfo]
groupByLicense = Foldable.foldl' (\m ipi -> insertWith (++) (InstalledPackageInfo.license ipi) [ipi] m) Map.empty

acceptableLicenses :: Set Cabal.License
acceptableLicenses = Set.fromList [Cabal.BSD2, Cabal.BSD3, Cabal.MIT, Cabal.ISC, Cabal.PublicDomain]

licenseMap :: Map Cabal.License String
licenseMap = Map.fromList
  [ (Cabal.BSD3,"BSD-3-Clause")
  , (Cabal.BSD2,"BSD-2-Clause")
  , (Cabal.MIT,"MIT")
  , (Cabal.ISC,"ISC")
  ]

haskellLicenseList :: Map Cabal.License [InstalledPackageInfo] -> String
haskellLicenseList byLicense = unlines $ fst $ do
  for_ (Map.toList byLicense) $ \(license, ipis) -> do
    put ""
    put $ "### " ++ case Map.lookup license licenseMap of
      Just nice -> "[" ++ nice ++ "](https://opensource.org/licenses/" ++ nice ++ ")"
      Nothing -> Cabal.display license
    put ""
    for_ (sortBy (compare `on` getName) ipis) $ \ipi -> do
      let name = getName ipi
      put $ "- [" ++ name ++ "](http://hackage.haskell.org/package/" ++ name ++ ") by " ++ getAuthorOrMaintainer ipi
  where
    getName = Cabal.display . {- Cabal.pkgName . -} InstalledPackageInfo.sourcePackageId
    getAuthor = unwords . lines . InstalledPackageInfo.author
    getMaintainer = unwords . lines . InstalledPackageInfo.maintainer
    getAuthorOrMaintainer ip
      | a == ""   = getMaintainer ip
      | otherwise = a
      where a = getAuthor ip

put :: String -> ([String],())
put x = ([x],())

nodeLicenseList :: Lazy.ByteString -> String
nodeLicenseList xs = case decode xs of
  Nothing -> error "Unable to parse node dependecies"
  Just (byPackage :: Map String (Map String String)) -> unlines $ fst $ do
    let flop (package,m) = (license, [(package, (publisher, repo, licenseFile))]) where
          license   = fromMaybe "" (Map.lookup "licenses" m)
          publisher = Map.lookup "publisher" m
          repo      = Map.lookup "repository" m
          licenseFile = fromMaybe "" (Map.lookup "licenseFile" m)
    let byLicense = Map.fromListWith (++) $ flop <$> Map.toList byPackage
    for_ (Map.toList byLicense) $ \(license, pkgs) -> do
       put ""
       put $ "### [" ++ license ++ "](https://opensource.org/licenses/" ++ license ++ ")"
       put ""
       for_ (sortBy (compare `on` fst) pkgs) $ \(pkg, (publisher, repo, licenseFile)) -> do
         let displayName = case repo of
               Just r -> "[" ++ pkg ++ "](" ++ r ++ ")"
               Nothing -> pkg
         put $ "- " ++ displayName ++ maybe "" (" by " ++) publisher ++ " [**[LICENSE]**](" ++ licenseFile ++ ")"

#if !MIN_VERSION_Cabal(2,0,0)
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif
