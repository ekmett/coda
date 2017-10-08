{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides a couple of ad hoc combinators that make it
-- easier to share code (at least in style) between 'toEncoding' and
-- 'toJSON' definitions for 'ToJSON'.
--------------------------------------------------------------------

module Coda.Util.Aeson 
  ( pattern JSON
  ) where

import Coda.Util.Instances () -- Void instances
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

pattern JSON :: (FromJSON a, ToJSON a, AsJSON t) => () => a -> t
pattern JSON a <- (preview _JSON -> Just a) where
  JSON a = _JSON # a
