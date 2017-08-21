{-# LANGUAGE LambdaCase #-}
module Main where

import Build_coda (buildDir, packageVersion)
import Data.List (intercalate, unwords)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Version
import System.Environment (getArgs, lookupEnv)

vsce :: FilePath
vsce = "node_modules/vsce/out/vsce"

ver :: String
ver = intercalate "." [ show x | x <- tail (versionBranch packageVersion) ]

package :: FilePath
package = buildDir </> ("coda-" ++ ver ++ ".vsix")

progress :: IO Progress -> IO ()
progress p = lookupEnv "CI" >>= \case
  Just "true" -> return ()
  _ -> do
    program <- progressProgram
    progressDisplay 0.5 (\s -> progressTitlebar s >> program s) p

main :: IO ()
main = do
 args <- getArgs
 putStrLn $ unwords ("build":args)
 shakeArgs shakeOptions { shakeFiles = buildDir, shakeThreads = 0, shakeProgress = progress, shakeLineBuffering = False } $ do
  phony "all" $ need [package]
  phony "copy" $ need ["install"]

  phony "install" $ do
    need [package]
    cmd "code" "--install-extension" [package]

  phony "package" $ need [package]

  phony "clean" $ removeFilesAfter "bin" ["//*"]

  want ["bin/coda" <.> exe]

  "node_modules/vscode/vscode.d.ts" %> \_ -> do
    need ["node_modules/vscode/package.json"]
    cmd "npm" "run-script" "update-vscode"

  "node_modules/vscode/package.json" %> \_ -> do
    need ["package.json"]
    cmd "npm" "install" "--ignore-scripts"

  "bin/coda" <.> exe %> \_ -> copyFileChanged (buildDir </> "coda" </> "coda" <.> exe) ("bin/coda" <.> exe)

  "bin/extension.js" %> \_ -> do
    need ["tsconfig.json","package.json","extension.ts","node_modules/vscode/vscode.d.ts"]
    cmd "npm" "run-script" "compile"

  vsce %> \_ -> need ["bin/extension.js"]

  package %> \out -> do
    need ["bin/extension.js", "bin/coda"]
    cmd vsce "package" "-o" [out]
