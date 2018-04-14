module Console (console) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Char
import System.Console.Haskeline

import Options
import Unicode

-- returns whether to carry on
executeCommand :: String -> InputT IO Bool
executeCommand "q" = return False
executeCommand _ = return True

console :: Options -> IO ()
console _opts = withUnicode $ runInputT defaultSettings loop

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
