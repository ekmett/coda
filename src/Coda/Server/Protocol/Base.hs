{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD2 (see the file LICENSE.md)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- JSON-RPC 2.0
--
-- http://www.jsonrpc.org/specification
-----------------------------------------------------------------------------

module Coda.Server.Protocol.Base
  (
    -- * JSON-RPC 2.0
    Id(..)
  , Request(Request, Notification, requestId, requestMethod, requestParams)
  , Response(..)
  , ResponseError(..)
  , ErrorCode
    ( ErrorCode
    , ParseError
    , InvalidRequest
    , MethodNotFound
    , InvalidParams
    , InternalError
    , ServerErrorStart
    , ServerErrorEnd
    , ServerNotInitialized
    , UnknownErrorCode
    , RequestCancelled
    )
  ) where

import Coda.Util.Aeson
import Control.Applicative
import Control.Comonad
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import Data.Ix
import Data.Monoid ((<>))
import Data.String
import Data.Text
import GHC.Generics

--------------------------------------------------------------------------------
-- JSON-RPC 2.0
--------------------------------------------------------------------------------

jsonRpcVersion :: Text
jsonRpcVersion = fromString "2.0"

--------------------------------------------------------------------------------
-- Id
--------------------------------------------------------------------------------

-- | A JSON-RPC message identifier
data Id
  = IntId !Int
  | TextId !Text
  deriving (Eq, Ord, Show, Data, Generic)

instance ToJSON Id where
  toJSON (IntId i) = Number $ fromIntegral i
  toJSON (TextId s) = String s
  toEncoding (IntId i) = int i
  toEncoding (TextId s) = text s

instance FromJSON Id where
  parseJSON a = IntId  <$> parseJSON a
            <|> TextId <$> parseJSON a

instance IsString Id where
  fromString = TextId . fromString

--------------------------------------------------------------------------------
-- ** Request
--------------------------------------------------------------------------------

-- |
-- http://www.jsonrpc.org/specification#request_object
data Request a = Request
  { requestId     :: !(Maybe Id)
  , requestMethod :: !Text
  , requestParams :: !a
  } deriving (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

instance FromJSON a => FromJSON (Request a) where
  parseJSON = withObject "Request" $ \v -> do
    ver <- v .: "jsonrpc" -- check for jsonprc validity
    guard (ver == jsonRpcVersion)
    Request <$> v .:? "id"
            <*> v .: "method"
            <*> parseMissingAsNull v "params"

instance ToJSON a => ToJSON (Request a) where
  toJSON (Request i m a)     = object $
    "jsonrpc" !~ jsonRpcVersion <> "id" ?~ i <> "method" !~ m <> "params" ??~ a
  toEncoding (Request i m a) = pairs $
    "jsonrpc" !~ jsonRpcVersion <> "id" ?~ i <> "method" !~ m <> "params" ??~ a

instance Comonad Request where
  extract (Request _ _ p) = p
  extend f w = w { requestParams = f w }

pattern Notification :: Text -> a -> Request a
pattern Notification method params = Request Nothing method params

--------------------------------------------------------------------------------
-- ** Response
--------------------------------------------------------------------------------

-- |
-- http://www.jsonrpc.org/specification#response_object
data Response e a = Response
  { responseId     :: !(Maybe Id)
  , responseResult :: !a
  , responseError  :: !(Maybe (ResponseError e))
  } deriving (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

instance (ToJSON e, ToJSON a) => ToJSON (Response e a) where
  toJSON (Response i r e) = object $
       "jsonrpc" !~ jsonRpcVersion
    <> "id"      ?~ i
    <> "result"  ??~ r
    <> "error"   ?= fmap toJSON e

  toEncoding (Response i r e) = pairs $
       "jsonrpc" !~ jsonRpcVersion
    <> "id"      ?~ i
    <> "result"  ??~ r
    <> "error"   ?= fmap toEncoding e

instance (FromJSON e, FromJSON a) => FromJSON (Response e a) where
  parseJSON = withObject "Response" $ \v -> do
    ver <- v .: "jsonrpc"
    guard (ver == jsonRpcVersion)
    Response
      <$> v .:? "id"
      <*> parseMissingAsNull v "result"
      <*> v .:? "error"

instance Comonad (Response e) where
  extract = responseResult
  extend f w = w { responseResult = f w }

instance Bifunctor Response where
  bimap f g (Response i r e) = Response i (g r) (fmap (fmap f) e)

instance Bifoldable Response where
  bifoldMap f g (Response _ r e) = g r <> foldMap (foldMap f) e

instance Bitraversable Response where
  bitraverse f g (Response i r e) = Response i <$> g r <*> traverse (traverse f) e

--------------------------------------------------------------------------------
-- ErrorCode
--------------------------------------------------------------------------------

-- | Defined in http://www.jsonrpc.org/specification#error_object
newtype ErrorCode = ErrorCode Int
  deriving (Show, Eq, Ord, Read, Bounded, Ix, Data, Generic)

instance FromJSON ErrorCode where
  parseJSON v = ErrorCode <$> parseJSON v

instance ToJSON ErrorCode where
  toJSON (ErrorCode e) = toJSON e
  toEncoding (ErrorCode e) = toEncoding e

pattern ParseError :: ErrorCode
pattern ParseError = ErrorCode (-32700)

pattern InvalidRequest :: ErrorCode
pattern InvalidRequest = ErrorCode (-32600)

pattern MethodNotFound :: ErrorCode
pattern MethodNotFound = ErrorCode (-32601)

pattern InvalidParams :: ErrorCode
pattern InvalidParams = ErrorCode (-32602)

pattern InternalError :: ErrorCode
pattern InternalError = ErrorCode (-32603)

pattern ServerErrorStart :: ErrorCode
pattern ServerErrorStart = ErrorCode (-32099)

pattern ServerErrorEnd :: ErrorCode
pattern ServerErrorEnd = ErrorCode (-32000)

pattern ServerNotInitialized :: ErrorCode
pattern ServerNotInitialized = ErrorCode (-32002)

pattern UnknownErrorCode :: ErrorCode
pattern UnknownErrorCode = ErrorCode (-32001);

pattern RequestCancelled :: ErrorCode
pattern RequestCancelled = ErrorCode (-32800);

--------------------------------------------------------------------------------
-- ResponseError
--------------------------------------------------------------------------------

-- |
-- http://www.jsonrpc.org/specification#error_object
data ResponseError a = ResponseError
  { responseErrorCode    :: !ErrorCode
  , responseErrorMessage :: !Text
  , responseErrorData    :: !a
  } deriving (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

instance FromJSON a => FromJSON (ResponseError a) where
  parseJSON = withObject "ResponseError" $ \v -> ResponseError
    <$> v .: "code"
    <*> v .: "message"
    <*> parseMissingAsNull v "data"

instance ToJSON a => ToJSON (ResponseError a) where
  toJSON (ResponseError c m d) = object $
    "code" !~ c <> "message" !~ m <> "data" ??~ d
  toEncoding (ResponseError c m d) = pairs $
    "code" !~ c <> "message" !~ m <> "data" ??~ d

instance Comonad ResponseError where
  extract = responseErrorData
  extend f w = w { responseErrorData = f w }
