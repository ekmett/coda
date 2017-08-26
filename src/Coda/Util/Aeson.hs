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
  ) where

import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Text

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

infixr 8 ?=, !=, !~, ?~

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

