#!/usr/bin/runhaskell
\begin{code}
{-# options_ghc -Wall -fno-warn-unused-binds #-}
module Main (main) where

import Distribution.Extra.Doctest (doctestsUserHooks)
import Distribution.Simple (defaultMainWithHooks, buildHook)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (FilePath)
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

run :: FilePath -> [String] -> IO ()
run cmd args = do
  (c, out, err) <- readProcessWithExitCode cmd args ""
  case c of
    ExitSuccess -> return ()
    ExitFailure e -> do
      hPutStrLn stderr $ "Command \"" ++ unwords (cmd:args) ++ "\" failed with exit code: " ++ show e
      hPutStrLn stdout out
      hPutStrLn stderr err
      exitWith $ ExitFailure e

main :: IO ()
main = defaultMainWithHooks $ my_hooks
  { buildHook = \pkg lbi hooks flags -> do
    buildHook my_hooks pkg lbi hooks flags
    -- run "vsce" ["package"]
  }
 where my_hooks = doctestsUserHooks "doctests"

\end{code}
