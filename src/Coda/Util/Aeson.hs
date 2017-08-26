{-# language TypeFamilies #-}
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
  ( FromHook(..)
  , ToHook(..)
  , (?~), (?=)
  , (??~)
  , parseMissingAsNull
  ) where

import Coda.Util.Instances () -- Void instances
import Data.Aeson
import Data.Aeson.Internal
import Data.Aeson.Encoding.Internal
import Data.Aeson.Types
import Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

parseMissingAsNull :: FromJSON a => Object -> Text -> Parser a
parseMissingAsNull m k = parseJSON (fromMaybe Null (HashMap.lookup k m)) <?> Key k

infixr 8 ?=, !=, !~, ?~, ??~

class Monoid (Ob v) => FromHook v where
  type Ob v :: *
  (!=) :: Text -> v -> Ob v

instance x ~ Value => FromHook (Encoding' x) where
  type Ob (Encoding' x) = Series
  t != a       = pair t a

instance FromHook Value where
  type Ob Value = [(Text, Value)]
  t != a       = [t .= a]

class Monoid t => ToHook t where
  (!~) :: ToJSON v => Text -> v -> t

instance ToHook Series where
 t !~ a       = pair t (toEncoding a)

instance x ~ (Text, Value) => ToHook [x] where
 t !~ a = [t .= toJSON a]

(?=) :: FromHook v => Text -> Maybe v -> Ob v
t ?= Just a  = t != a
_ ?= Nothing = mempty

(?~) :: (ToJSON v, ToHook t) => Text -> Maybe v -> t
t ?~ Just a  = t !~ a
_ ?~ Nothing = mempty

(??~) :: (ToJSON a, ToHook r) => Text -> a -> r
t ??~ a = case toJSON a of
  Null -> mempty
  x    -> t !~ x

