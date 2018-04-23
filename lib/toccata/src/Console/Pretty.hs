{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language DefaultSignatures #-}

module Console.Pretty
  (
  -- * options
    FancyOptions(..)
  , HasFancyOptions(..)
  , parseFancyOptions
  -- * pretty printing
  , putFancy
  , hPutFancy
  ) where

import Control.Monad.IO.Class
import Control.Lens
import Data.Char (toLower)
import Data.Default.Class
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text as RenderText
import Data.Text.Prettyprint.Doc.Render.Terminal as RenderTerminal
import GHC.IO.Handle
import Options.Applicative as Options
import System.Console.ANSI (hSupportsANSI)
import System.IO (Handle, stdout)
#ifdef MIN_VERSION_terminfo
import System.Console.Terminfo.Base (setupTermFromEnv, getCapability)
import System.Console.Terminfo.Cursor (termColumns)
#endif
import Text.Read (readMaybe)

data FancyOptions
  = FancyOptions
  { _fancyColor :: Maybe Bool
  , _fancyWidth :: Maybe Int
  }
  deriving Show

makeClassy ''FancyOptions

instance Default FancyOptions where
  def = FancyOptions Nothing Nothing

parseFancyOptions :: Options.Parser FancyOptions
parseFancyOptions = FancyOptions
  <$> option colorOpt colorPrefs
  <*> option widthOpt widthPrefs
  where
    colorPrefs
       = long "ansi-color"
      <> short 'c'
      <> metavar "SETTING"
      <> value Nothing
      <> help "Emit ANSI from the console: 'always', 'never' or 'auto'"
    colorOpt = eitherReader $ \s -> case map toLower s of
      "always" -> pure $ Just True
      "never"  -> pure $ Just False
      "auto"   -> pure Nothing
      _        -> fail "Expected --ansi-color=always|never|auto"
    widthOpt = eitherReader $ \s -> case map toLower s of
      "auto" -> pure Nothing
      _      -> case readMaybe s of
        Just a  -> Right (Just a)
        Nothing -> fail "Expected number or 'auto'"
    widthPrefs
       = long "console-width"
      <> short 'w'
      <> metavar "WIDTH"
      <> value Nothing
      <> help "Width of the console in characters or 'auto'"

defaultCols :: Int
defaultCols = 78

fansi :: FancyOptions -> Handle -> IO Bool
fansi opts h = maybe (hSupportsANSI h) pure (_fancyColor opts)

fcols :: FancyOptions -> Handle -> IO Int
fcols opts h = maybe query pure (_fancyWidth opts) where
#ifdef MIN_VERSION_terminfo
  query = do
    tty <- hIsTerminalDevice h
    if tty then do
      term <- setupTermFromEnv
      return $ fromMaybe defaultCols $ getCapability term termColumns
    else return defaultCols
#else
  query = return defaultCols
#endif

flayoutOptions :: FancyOptions -> Handle -> IO LayoutOptions
flayoutOptions opts h = do
  cols <- fcols opts h
  return defaultLayoutOptions { layoutPageWidth = AvailablePerLine cols 1.0 }

putFancy :: MonadIO m => FancyOptions -> Doc AnsiStyle -> m ()
putFancy opts = hPutFancy opts stdout

hPutFancy :: MonadIO m => FancyOptions -> Handle -> Doc AnsiStyle -> m ()
hPutFancy opts h doc = liftIO $ do
  b <- fansi opts h
  layout <- flayoutOptions opts h
  if b then RenderTerminal.renderIO h $ layoutPretty layout doc
       else RenderText.renderIO h $ layoutPretty layout $ unAnnotate doc
