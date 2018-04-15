{-# language TemplateHaskell #-}
{-# language DefaultSignatures #-}

module Console.Pretty
  ( 
  -- * styles
    CodeStyle(..)
  -- * options
  , FancyOptions(..)
  , HasFancyOptions(..)
  , parseFancyOptions
  -- * pretty printing
  , putFancy
  , hPutFancy
  -- * pretty printing class
  , Fancy(..)
  , pp
  -- * internals
  , ansi
  , pp_ -- convenience
  ) where

import Control.Monad.IO.Class
import Control.Lens
import Data.Char (toLower)
import Data.Default.Class
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text as RenderText
import Data.Text.Prettyprint.Doc.Render.Terminal as RenderTerminal
import GHC.Arr (Ix(..))
import Options.Applicative as Options
import System.Console.ANSI (hSupportsANSI)
import System.IO (Handle, stdout)

data CodeStyle
  = StyleVar
  | StyleOp
  | StyleKeyword
  deriving (Eq,Ord,Show,Read,Ix,Bounded,Enum)

ansi :: CodeStyle -> AnsiStyle
ansi StyleVar = bold
ansi StyleOp = color Yellow
ansi StyleKeyword = color Green

-- ansi er.. fansi er.. fancy
class Fancy a where
  fancy :: a -> Doc CodeStyle
  default fancy :: Pretty a => a -> Doc CodeStyle
  fancy = pretty

data FancyOptions
  = FancyOptions
  { _fancyColor :: Maybe Bool }
  deriving Show

makeClassy ''FancyOptions

instance Default FancyOptions where
  def = FancyOptions Nothing

parseFancyOptions :: Options.Parser FancyOptions
parseFancyOptions = FancyOptions 
  <$> option colorOpt colorPrefs
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
      _ -> fail "Expected --ansi-color=always|never|auto"

fansi :: FancyOptions -> Handle -> IO Bool
fansi opts h = maybe (hSupportsANSI h) pure (_fancyColor opts)
 
putFancy :: MonadIO m => FancyOptions -> Doc CodeStyle -> m ()
putFancy opts = hPutFancy opts stdout

hPutFancy :: MonadIO m => FancyOptions -> Handle -> Doc CodeStyle -> m ()
hPutFancy opts h doc = liftIO $ do
  b <- fansi opts h
  if b then RenderTerminal.hPutDoc h (fmap ansi doc)
       else RenderText.hPutDoc h doc

pp :: (MonadIO m, Fancy a) => FancyOptions -> a -> m ()
pp opts = putFancy opts . fancy

-- for quick testing
pp_ :: (MonadIO m, Fancy a) => a -> m ()
pp_ = pp def

