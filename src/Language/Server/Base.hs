{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}
{-# language DuplicateRecordFields #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

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
-- <http://www.jsonrpc.org/specification>
-----------------------------------------------------------------------------

module Language.Server.Base
  (
    -- * JSON-RPC 2.0
    Id(..)

    -- * Requests
  , Request(..)

    -- * Reponses
  , Response(..)
  , ResponseError(..)

    -- ** Error Codes
  , ErrorCode (..)
  , pattern ParseError
  , pattern InvalidRequest
  , pattern MethodNotFound
  , pattern InvalidParams
  , pattern InternalError
  , pattern ServerErrorStart
  , pattern ServerErrorEnd
  , pattern ServerNotInitialized
  , pattern UnknownErrorCode

  , HasMethod(..)
  , HasParams(..)
  , HasId(..)
  , HasMessage(..)
  , HasCode(..)
  , HasData(..)
  , HasError(..)
  , HasResult(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Internal
import Data.Data
import Data.Hashable
import Data.Ix
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.String
import Data.Text
import GHC.Generics
import Language.Server.TH

--------------------------------------------------------------------------------
-- JSON-RPC 2.0
--------------------------------------------------------------------------------

jsonRpcVersion :: Text
jsonRpcVersion = fromString "2.0"

--------------------------------------------------------------------------------
-- Id
--------------------------------------------------------------------------------

-- | A JSON-RPC message identifier
data Id = IntId !Int | TextId !Text
  deriving (Eq, Ord, Show, Read, Data, Generic)

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

instance Hashable Id where
  hashWithSalt i (IntId j) = hashWithSalt i j
  hashWithSalt i (TextId j) = hashWithSalt i j
  hash (IntId j) = hash j
  hash (TextId j) = hash j

--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

-- |
-- <http://www.jsonrpc.org/specification#request_object>
--
-- @
-- interface RequestMessage extends Message {
--   id: number | string; -- missing id is a notification, both are unified here
--   method: string;
--   params?: any
-- }
-- @
data Request = Request
  { _id     :: !(Maybe Id)
  , _method :: !Text
  , _params :: !(Maybe Value)
  } deriving (Eq, Show, Read, Data, Generic)

instance FromJSON Request where
  parseJSON = withObject "Request" $ \v -> do
    ver <- v .: "jsonrpc" -- check for jsonprc validity
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version"
    Request <$> v .:? "id"
            <*> v .: "method"
            <*> v .:? "params"

instance ToJSON Request where
  toJSON (Request mi m mp) = object $
    [ "jsonrpc" .= jsonRpcVersion
    , "method" .= m
    ] ++ catMaybes
    [ ("id" .=) <$> mi
    , ("params" .=) <$> mp
    ]
  toEncoding (Request mi m mp) = pairs
    $ "jsonrpc" .= jsonRpcVersion
   <> foldMap ("id" .=) mi
   <> "method" .= m
   <> foldMap ("params" .=) mp

instance Hashable Request

lenses ''Request

--------------------------------------------------------------------------------
-- ErrorCode
--------------------------------------------------------------------------------

-- | <http://www.jsonrpc.org/specification#error_object>
newtype ErrorCode = ErrorCode Int
  deriving (Show, Eq, Ord, Read, Bounded, Ix, Data, Generic)

instance FromJSON ErrorCode where
  parseJSON v = ErrorCode <$> parseJSON v

instance ToJSON ErrorCode where
  toJSON (ErrorCode e) = toJSON e
  toEncoding (ErrorCode e) = toEncoding e

instance Hashable ErrorCode where
  hashWithSalt i (ErrorCode e) = hashWithSalt i e

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
pattern UnknownErrorCode = ErrorCode (-32001)

--------------------------------------------------------------------------------
-- ResponseError
--------------------------------------------------------------------------------

-- |
-- <http://www.jsonrpc.org/specification#error_object>
--
-- @
-- interface ResponseError<D> {
--   code: number;
--   message: string;
--   data?: D;
-- }
-- @

data ResponseError = ResponseError
  { _code    :: !ErrorCode
  , _message :: !Text
  , _data    :: !(Maybe Value)
  } deriving (Eq, Show, Read, Data, Generic)

jsonOmit ''ResponseError
lenses ''ResponseError

instance Hashable ResponseError

--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

-- |
-- <http://www.jsonrpc.org/specification#response_object>
--
-- @
-- interface ResponseMessage extends Message {
--   id: number | string | null;
--   result?: any;
--   error?: ResponseError<any>;
-- }
-- @
data Response = Response
  { _id     :: !(Maybe Id)
  , _result :: !(Maybe Value)
  , _error  :: !(Maybe ResponseError)
  } deriving (Eq, Show, Read, Data, Generic)

instance ToJSON Response where
  toJSON (Response mi m mp) = object $
    [ "jsonrpc" .= jsonRpcVersion
    , "id" .= mi
    ] ++ catMaybes
    [ ("result" .=) <$> m
    , ("error" .=) <$> mp
    ]
  toEncoding (Response mi m mp) = pairs
    $ "jsonrpc" .= jsonRpcVersion
   <> "id" .= mi
   <> foldMap ("result" .=) m
   <> foldMap ("error" .=) mp

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v -> do
    ver <- v .: "jsonrpc"
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version" <?> Key "jsonrpc"
    Response
      <$> v .: "id"
      <*> v .:? "result"
      <*> v .:? "error"

instance Hashable Response

lenses ''Response
