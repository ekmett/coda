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
-- <http://www.jsonrpc.org/specification>
-----------------------------------------------------------------------------

module Coda.Message.Base
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

    -- * Overloading
  , HasParams(..)
  , HasMethod(..)
  ) where

import Coda.Util.Aeson
import Control.Applicative
import Control.Lens.Operators ((??))
import Control.Lens.Combinators
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Internal
import Data.Data
import Data.Hashable
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
-- Overloads
--------------------------------------------------------------------------------

class HasMethod t where
  method :: Lens' t Text

class HasParams t where
  params :: Lens' t (Maybe Value)

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
  { requestId     :: !(Maybe Id)
  , requestMethod :: !Text
  , requestParams :: !(Maybe Value)
  } deriving (Eq, Show, Read, Data, Generic)

instance HasMethod Request where
  method f (Request i m p) = (Request i ?? p) <$> f m

instance HasParams Request where
  params f (Request i m p) = Request i m <$> f p

instance FromJSON Request where
  parseJSON = withObject "Request" $ \v -> do
    ver <- v .: "jsonrpc" -- check for jsonprc validity
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version"
    Request <$> v .:? "id"
            <*> v .: "method"
            <*> v .:? "params"

instance ToJSON Request where
  toJSON (Request i m a)     = object $
    "jsonrpc" !~ jsonRpcVersion <> "id" ?~ i <> "method" !~ m <> "params" ?~ a
  toEncoding (Request i m a) = pairs $
    "jsonrpc" !~ jsonRpcVersion <> "id" ?~ i <> "method" !~ m <> "params" ?~ a

instance Hashable Request

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
  { responseId     :: !(Maybe Id)
  , responseResult :: !(Maybe Value)
  , responseError  :: !(Maybe ResponseError)
  } deriving (Eq, Show, Read, Data, Generic)

instance ToJSON Response where
  toJSON (Response i r e) = object $
       "jsonrpc" !~ jsonRpcVersion
    <> "id"      !~ i
    <> "result"  ?~ r
    <> "error"   ?~ e

  toEncoding (Response i r e) = pairs $
       "jsonrpc" !~ jsonRpcVersion
    <> "id"      !~ i
    <> "result"  ?~ r
    <> "error"   ?~ e

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v -> do
    ver <- v .: "jsonrpc"
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version" <?> Key "jsonrpc"
    Response
      <$> v .: "id"
      <*> v .:? "result"
      <*> v .:? "error"

instance Hashable Response

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
  { responseErrorCode    :: !ErrorCode
  , responseErrorMessage :: !Text
  , responseErrorData    :: !(Maybe Value)
  } deriving (Eq, Show, Read, Data, Generic)

instance FromJSON ResponseError where
  parseJSON = withObject "ResponseError" $ \v -> ResponseError
    <$> v .:  "code"
    <*> v .:  "message"
    <*> v .:? "data"

instance ToJSON ResponseError where
  toJSON (ResponseError c m d) = object $
    "code" !~ c <> "message" !~ m <> "data" ?~ d
  toEncoding (ResponseError c m d) = pairs $
    "code" !~ c <> "message" !~ m <> "data" ?~ d

instance Hashable ResponseError
