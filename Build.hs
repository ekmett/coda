{-# LANGUAGE LambdaCase #-}
module Main where

import Build_coda (buildDir, packageVersion)
import Data.List (intercalate)
import Data.Foldable (for_)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Version
import System.Directory (makeAbsolute)
import System.Environment (lookupEnv)

ver :: String
ver = intercalate "." [ show x | x <- tail (versionBranch packageVersion) ]

package :: FilePath
package = buildDir </> ("coda-" ++ ver ++ ".vsix")

extDir :: FilePath
extDir = buildDir </> "extension"

vsce :: FilePath
vsce = extDir </> "node_modules/vsce/out/vsce"

progress :: IO Progress -> IO ()
progress p = lookupEnv "CI" >>= \case
  Just "true" -> return ()
  _ -> do
    program <- progressProgram
    progressDisplay 0.5 (\s -> progressTitlebar s >> program s) p

main :: IO ()
main = do
 absolutePackage <- makeAbsolute package
 absoluteVscePath <- makeAbsolute $ extDir </> "node_modules/vsce/out"
 extensionFiles <- getDirectoryFilesIO "extension" ["//*"]
 markdownFiles <- getDirectoryFilesIO "" ["*.md"]
 shakeArgs shakeOptions
   { shakeFiles = buildDir
   , shakeThreads = 0
   , shakeProgress = progress
   , shakeLineBuffering = False
   , shakeVerbosity = Loud
   } $ do
  phony "all" $ need [package]

  phony "install" $ do
    need ["copy"]
    need ["register"]

  phony "package" $ need [package]

  phony "test" $ do
    need $ map (extDir </>) extensionFiles
    cmd (Cwd extDir) "npm" "run-script" "lint"

  phony "copy" $ pure ()
  phony "register" $ do
    need [package]
    cmd "code" "--install-extension" [package]

  want [extDir </> "bin/coda" <.> exe]

  extDir </> "node_modules/vscode/vscode.d.ts" %> \_ -> do
    need [extDir </> "node_modules/vscode/package.json"]
    cmd (Cwd extDir) "npm" "run-script" "update-vscode"


  extDir </> "node_modules/vscode/package.json" %> \_ -> do
    need [extDir </> "package.json"]
    cmd (Cwd extDir) "npm" "install" "--ignore-scripts"

  extDir </> "bin/coda" <.> exe %> \_ -> copyFileChanged (buildDir </> "coda" </> "coda" <.> exe) (extDir </> "bin/coda" <.> exe)

  extDir </> "bin/extension.js" %> \_ -> do
    need [extDir </> "node_modules/vscode/vscode.d.ts", extDir </> "package.json"]
    cmd (Cwd extDir) "npm" "run-script" "compile"

  vsce %> \_ -> need [extDir </> "bin/extension.js"]

  package %> \_ -> do
    need $ [extDir </> "bin/extension.js", extDir </> "bin/coda", vsce]
        ++ map (extDir </>) (extensionFiles ++ markdownFiles)
    cmd (AddPath [absoluteVscePath] []) (Cwd extDir) "vsce" "package" "-o" [makeRelative extDir absolutePackage]

  for_ extensionFiles $ \file -> extDir </> file %> \out -> copyFile' ("extension" </> file) out
  for_ markdownFiles $ \file -> extDir </> file %> \out -> copyFile' file out
