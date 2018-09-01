---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Syntax.FromText
  ( FromText(..)
  ) where

import Data.Text

class FromText a where
  fromText :: Text -> a

instance FromText Text where
  fromText = id
