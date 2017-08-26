{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Data types the Language Server Protocol
--------------------------------------------------------------------------------

module Coda.Server.Protocol.Language.Base
  ( pattern CancelRequest, _CancelRequest
  , DocumentUri
  , Position(..)
  , Range(..)
  ) where

import Coda.Server.Protocol.Base
import Coda.Util.Aeson
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Data
import Data.Ix
import Data.Monoid
import Data.Text as Text
import GHC.Generics

-- | Language Server Protocol: cancellation notification
pattern CancelRequest :: Id -> Request Value
pattern CancelRequest identifier <- Request Nothing "$/cancelRequest" (Just (fromJSON -> Success identifier)) where
  CancelRequest identifier = Request Nothing "$/cancelRequest" (Just (toJSON (identifier)))

_CancelRequest :: Prism' (Request Value) Id
_CancelRequest = prism' (Request Nothing "$/cancelRequest" . Just . toJSON) $ \case
  Request Nothing "$/cancelRequest" (Just (fromJSON -> Success a)) -> Just a
  _ -> Nothing
  
type DocumentUri = Text

data LineEnding = LineEndingCR | LineEndingLF | LineEndingCRLF
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

instance ToJSON LineEnding where
  toJSON LineEndingCR   = String "\r"
  toJSON LineEndingLF   = String "\n"
  toJSON LineEndingCRLF = String "\r\n"
  toEncoding LineEndingCR   = string "\r"
  toEncoding LineEndingLF   = string "\n"
  toEncoding LineEndingCRLF = string "\r\n"

instance FromJSON LineEnding where
  parseJSON (String "\r")   = pure LineEndingCR
  parseJSON (String "\n")   = pure LineEndingLF 
  parseJSON (String "\r\n") = pure LineEndingCRLF
  parseJSON _ = mzero

data Position = Position
  { positionLine      :: !Int -- ^ 0-based line number
  , positionCharacter :: !Int -- ^ 0-based count of utf-16 words (not code-points!)
  } deriving (Eq,Ord,Show,Read,Data,Generic)

instance ToJSON Position where
  toJSON     (Position l c) = object $ "line" !~ l <> "character" !~ c
  toEncoding (Position l c) = pairs  $ "line" !~ l <> "character" !~ c

instance FromJSON Position where
  parseJSON = withObject "Position" $ \v -> Position
    <$> v .: "line"
    <*> v .: "character"

data Range = Range
  { rangeStart :: !Int
  , rangeEnd :: !Int
  } deriving (Eq,Ord,Show,Read,Data,Generic)

instance ToJSON Range where
  toJSON     (Range s e) = object $ "start" !~ s <> "end" !~ e
  toEncoding (Range s e) = pairs  $ "start" !~ s <> "end" !~ e

instance FromJSON Range where
  parseJSON = withObject "Range" $ \v -> Range
    <$> v .: "start"
    <*> v .: "end"
