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
  , pattern Value_
  , pattern Number_
  , pattern Double
  , pattern Integer
  , pattern Integral
  , pattern Primitive
  , pattern Bool_
  , pattern String_
  , pattern Null_
  ) where

import Coda.Util.Instances () -- Void instances
import Control.Lens.Combinators
import Control.Lens.Operators ((#))
import Data.Aeson
import Data.Aeson.Lens
import Data.Scientific
import Data.Text

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

pattern JSON :: (FromJSON a, ToJSON a, AsJSON t) => () => a -> t
pattern JSON a <- (preview _JSON -> Just a) where
  JSON a = _JSON # a

pattern Value_ :: (FromJSON a, ToJSON a) => () => a -> Value
pattern Value_ a <- (fromJSON -> Success a) where
  Value_ a = toJSON a 

pattern Number_ :: AsNumber t => Scientific -> t
pattern Number_ n <- (preview _Number -> Just n) where
  Number_ n = _Number # n

pattern Double :: AsNumber t => Double -> t
pattern Double d <- (preview _Double -> Just d) where
  Double d = _Double # d

pattern Integer :: AsNumber t => Integer -> t
pattern Integer i <- (preview _Integer -> Just i) where
  Integer i = _Integer # i

pattern Integral :: (AsNumber t, Integral a) => a -> t
pattern Integral d <- (preview _Integral -> Just d) where
  Integral d = _Integral # d

pattern Primitive :: AsPrimitive t => Primitive -> t
pattern Primitive p <- (preview _Primitive -> Just p) where
  Primitive p = _Primitive # p

pattern Bool_ :: AsPrimitive t => Bool -> t
pattern Bool_ b <- (preview _Bool -> Just b) where
  Bool_ b = _Bool # b

pattern String_ :: AsPrimitive t => Text -> t
pattern String_ p <- (preview _String -> Just p) where
  String_ p = _String # p

pattern Null_ :: AsPrimitive t => t
pattern Null_ <- (preview _Null -> Just ()) where
  Null_ = _Null # ()
