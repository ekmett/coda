module Console (console) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)
import Data.Char
import System.Console.Haskeline

import Console.Completion
import Console.Options
import Console.Unicode
import Version

-- returns whether to carry on
executeCommand :: String -> InputT IO Bool
executeCommand "q" = return False
executeCommand _ = return True

heading :: String
heading = "Coda " ++ version

console :: ConsoleOptions -> IO ()
console opts = withUnicode $ do
  unless (opts^.consoleOptionsNoHeading) $ putStrLn heading
  runInputT settings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "λ> "
  case Prelude.dropWhile isSpace <$> minput of
    Nothing      -> return ()
    Just "quit"  -> return ()
    Just (':':cmd) -> do
      b <- executeCommand cmd
      when b loop
    Just ""      -> loop
    Just input   -> do
      liftIO $ putStrLn input
      loop
