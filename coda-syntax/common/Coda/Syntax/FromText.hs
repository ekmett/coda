module Coda.Syntax.FromText
  ( FromText(..)
  ) where

import Data.Text

class FromText a where
  fromText :: Text -> a

instance FromText Text where
  fromText = id
