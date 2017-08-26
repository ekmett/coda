module Coda.Console 
  ( heading
  , console
  ) where
  
import Coda.Console.Command
import Coda.Console.Completion
import Coda.Console.Options
import Coda.Console.Unicode
import Coda.Version
import Data.Char
import Control.Exception.Lens
import Control.Lens
import Control.Monad.State
import System.Console.Haskeline
import System.Exit.Lens

heading :: String
heading = "Coda, version " ++ version ++ ": http://github.com/ekmett/coda/  :? for help"

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
      lift $ handling (filtered (hasn't _ExitCode)) (liftIO . print) $ executeCommand cmd
      loop
    Just ""      -> loop
    Just input   -> do
      outputStrLn $ "Input was: " ++ input
      loop
