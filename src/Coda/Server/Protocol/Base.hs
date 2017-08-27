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

module Coda.Server.Protocol.Base
  (
    -- * JSON-RPC 2.0
    Id(..)
    -- * Requests
  , Request(..)
  , Request_
    -- ** Notifications
  , pattern RequestNotification, _RequestNotification
  , Notification(..)
  , Notification_
    -- * Reponses
  , Response(..)
  , Response_
  , ResponseError(..)
  , ResponseError_
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
  , pattern RequestCancelled
    -- * Overloading
  , HasId(..)
  , HasParams(..)
  , HasMethod(..)
  ) where

import Coda.Util.Aeson
import Control.Applicative
import Control.Comonad
import Control.Lens.Operators ((<&>), (??))
import Control.Lens.Combinators
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Internal
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import Data.Hashable
import Data.Hashable.Lifted
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

instance Hashable Id where
  hashWithSalt i (IntId j) = hashWithSalt i j
  hashWithSalt i (TextId j) = hashWithSalt i j
  hash (IntId j) = hash j
  hash (TextId j) = hash j

--------------------------------------------------------------------------------
-- Nil
--------------------------------------------------------------------------------

data Nil = Nil
  deriving (Eq, Ord, Show, Data, Generic, Ix, Bounded)

instance ToJSON Nil where
  toJSON Nil = Null
  toEncoding Nil = null_

instance FromJSON Nil where
  parseJSON Null = pure Nil
  parseJSON _ = fail "non-null"

instance Hashable Nil where
  hashWithSalt i Nil = i
  hash Nil = 0

--------------------------------------------------------------------------------
-- Overloads
--------------------------------------------------------------------------------

class HasId t where
  id_ :: Lens (t a c) (t b c) a b

class HasMethod t where
  method :: Lens' t Text

class HasParams f where
  params :: Lens (f a) (f b) a b

--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

-- |
-- <http://www.jsonrpc.org/specification#request_object>
--
-- @'Request' 'Id' 'Value'@ models
--
-- @
-- interface RequestMessage extends Message {
--   id: number | string;
--   method: string;
--   params?: any
-- }
-- @
--
-- @'Request' ('Maybe' 'Id') 'Value'@ models either a 'RequestMessage' or a 'Notification'.
data Request i a = Request
  { requestId     :: !i
  , requestMethod :: !Text
  , requestParams :: !a
  } deriving (Eq, Ord, Show, Data, Generic, Generic1, Functor, Foldable, Traversable)

type Request_ = Request Id Value

instance HasId Request where
  id_ f (Request i m p) = f i <&> \i' -> Request i' m p

instance HasMethod (Request i a) where
  method f (Request i m p) = (Request i ?? p) <$> f m

instance HasParams (Request i) where
  params f (Request i m p) = Request i m <$> f p

instance (FromJSON i, FromJSON a) => FromJSON (Request i a) where
  parseJSON = withObject "Request" $ \v -> do
    ver <- v .: "jsonrpc" -- check for jsonprc validity
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version"
    Request <$> parseMissingAsNull v "id"
            <*> v .: "method"
            <*> parseMissingAsNull v "params"

instance (ToJSON i, ToJSON a) => ToJSON (Request i a) where
  toJSON (Request i m a)     = object $
    "jsonrpc" !~ jsonRpcVersion <> "id" ??~ i <> "method" !~ m <> "params" ??~ a
  toEncoding (Request i m a) = pairs $
    "jsonrpc" !~ jsonRpcVersion <> "id" ??~ i <> "method" !~ m <> "params" ??~ a

instance Comonad (Request i) where
  extract (Request _ _ p) = p
  extend f w = w { requestParams = f w }

instance Bitraversable Request where
  bitraverse f g (Request i m a) =
    (\i' a' -> Request i' m a') <$> f i <*> g a

instance Bifoldable Request where
  bifoldMap f g (Request i _ a) = f i <> g a

instance Bifunctor Request where
  bimap f g (Request i m a) = Request (f i) m (g a)

instance Hashable2 Request where
  liftHashWithSalt2 f g s (Request i m a) = g (hashWithSalt (f s i) m) a

instance Hashable i => Hashable1 (Request i) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable i, Hashable a) => Hashable (Request i a) where
  hashWithSalt = hashWithSalt2

--------------------------------------------------------------------------------
-- Notification
--------------------------------------------------------------------------------

-- | JSON-RPC 2.0 notifications
--
-- @
-- interface NotificationMessage extends Message {
--   method: string;
--   params?: any
-- }
-- @
--
-- 'Notification' is isomorphic to 'Request Nil'
data Notification a = Notification
  { notificationMethod :: !Text
  , notificationParams :: !a
  } deriving (Eq, Ord, Show, Data, Generic, Generic1, Functor, Foldable, Traversable)

type Notification_ = Notification Value

instance HasMethod (Notification a) where
  method f (Notification m p) = (Notification ?? p) <$> f m

instance HasParams Notification where
  params f (Notification m p) = Notification m <$> f p

instance FromJSON a => FromJSON (Notification a) where
  parseJSON = withObject "Notification" $ \v -> do
    ver <- v .: "jsonrpc" -- check for jsonprc validity
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version" <?> Key "jsonrpc"
    Notification
      <$> v .: "method"
      <*> parseMissingAsNull v "params"

instance ToJSON a => ToJSON (Notification a) where
  toJSON (Notification m a)     = object $
    "jsonrpc" !~ jsonRpcVersion <> "method" !~ m <> "params" ??~ a
  toEncoding (Notification m a) = pairs $
    "jsonrpc" !~ jsonRpcVersion <> "method" !~ m <> "params" ??~ a

instance Comonad Notification where
  extract = notificationParams
  extend f w = w { notificationParams = f w }

_RequestNotification :: Prism' (Request (Maybe Id) a) (Notification a)
_RequestNotification = prism (\(Notification m a) -> Request Nothing m a) $ \w@(Request i m p) -> case i of
  Nothing -> Right (Notification m p)
  _       -> Left w

pattern RequestNotification :: Text -> a -> Request (Maybe Id) a
pattern RequestNotification m p = Request Nothing m p

instance Hashable1 Notification where
  liftHashWithSalt f s (Notification m a) = f (hashWithSalt s m) a

instance Hashable a => Hashable (Notification a) where
  hashWithSalt = hashWithSalt1

--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

-- |
-- <http://www.jsonrpc.org/specification#response_object>
--
-- @'Response (Maybe Id) Value' models
--
-- @
-- interface ResponseMessage extends Message {
--   id: number | string | null;
--   result?: any;
--   error?: ResponseError<any>;
-- }
-- @
data Response e i a = Response
  { responseId     :: !i
  , responseResult :: !a
  , responseError  :: !(Maybe (ResponseError e))
  } deriving (Eq, Show, Data, Generic, Generic1, Functor, Foldable, Traversable)

type Response_ = Response Value (Maybe Id) Value

instance (ToJSON e, ToJSON i, ToJSON a) => ToJSON (Response e i a) where
  toJSON (Response i r e) = object $
       "jsonrpc" !~ jsonRpcVersion
    <> "id"      !~ i
    <> "result"  ??~ r
    <> "error"   ?= fmap toJSON e

  toEncoding (Response i r e) = pairs $
       "jsonrpc" !~ jsonRpcVersion
    <> "id"      !~ i
    <> "result"  ??~ r
    <> "error"   ?= fmap toEncoding e

instance (FromJSON e, FromJSON i, FromJSON a) => FromJSON (Response e i a) where
  parseJSON = withObject "Response" $ \v -> do
    ver <- v .: "jsonrpc"
    when (ver /= jsonRpcVersion) $ fail "invalid JSON-RPC version" <?> Key "jsonrpc"
    Response
      <$> parseMissingAsNull v "id"
      <*> parseMissingAsNull v "result"
      <*> v .:? "error"

instance HasId (Response e) where
  id_ f (Response i r e) = f i <&> \i' -> Response i' r e

instance Comonad (Response e i) where
  extract = responseResult
  extend f w = w { responseResult = f w }

instance Bifunctor (Response e) where
  bimap f g (Response i r e) = Response (f i) (g r) e

instance Bifoldable (Response e) where
  bifoldMap f g (Response i r _) = f i <> g r

instance Bitraversable (Response e) where
  bitraverse f g (Response i r e) = (\i' r' -> Response i' r' e) <$> f i <*> g r

instance Hashable e => Hashable2 (Response e) where
  liftHashWithSalt2 f g s (Response i r e) = hashWithSalt (g (f s i) r) e

instance (Hashable e, Hashable i) => Hashable1 (Response e i) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable e, Hashable i, Hashable a) => Hashable (Response e i a) where
  hashWithSalt = hashWithSalt2

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

pattern RequestCancelled :: ErrorCode
pattern RequestCancelled = ErrorCode (-32800)

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

data ResponseError a = ResponseError
  { responseErrorCode    :: !ErrorCode
  , responseErrorMessage :: !Text
  , responseErrorData    :: !a
  } deriving (Eq, Ord, Show, Data, Generic, Generic1)

type ResponseError_ = ResponseError Value

instance FromJSON a => FromJSON (ResponseError a) where
  parseJSON = withObject "ResponseError" $ \v -> ResponseError
    <$> v .:  "code"
    <*> v .:  "message"
    <*> parseMissingAsNull v "data"

instance ToJSON a => ToJSON (ResponseError a) where
  toJSON (ResponseError c m d) = object $
    "code" !~ c <> "message" !~ m <> "data" ??~ d
  toEncoding (ResponseError c m d) = pairs $
    "code" !~ c <> "message" !~ m <> "data" ??~ d

instance Hashable1 ResponseError

instance Hashable a => Hashable (ResponseError a)
