{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}
{-# language TypeSynonymInstances #-}
{-# language DuplicateRecordFields #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -O0 #-}

--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- <http://www.jsonrpc.org/specification JSON-RPC 2.0> and the
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md Language Server Protocol>
--------------------------------------------------------------------------------

module Language.Server.Protocol
  (
    -- * JSON-RPC 2.0
    Id(..)

    -- ** Requests
  , Request(..)

    -- ** Reponses
  , Response(..)
  , ResponseError(..)

    -- *** Error Codes
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

  -- * Language Server Protocol
  -- ** Cancellation Support
  , pattern CancelRequest
  , pattern RequestCancelled
  , cancelledResponse
  -- ** Data Types

  -- *** DocumentUris
  , DocumentUri(..)
  , pattern File

  -- *** Positions
  , Position(..)
  , subtractPosition
  , Range(..)
  , rangeSize
  , Location(..)

  -- *** Diagnostics
  , Diagnostic(..)
  , Severity(..)
  , pattern Information
  , pattern Hint
  , pattern Warning
  , pattern Error

  , Command(..)

  , TextEdit(..)

  , TextDocumentIdentifier(..)
  , VersionedTextDocumentIdentifier(..)

  -- *** Contents
  , TextDocumentItem(..)
  , TextDocumentEdit(..)

  , WorkspaceEdit(..)
  , DocumentFilter(..)
  , TextDocumentPositionParams(..), TDPP
  -- ** Protocol
  -- *** 'initialize'
  , pattern Initialize
  , InitializeParams(..)
  , ClientCapabilities(..)
  , WorkspaceClientCapabilities
  , TextDocumentClientCapabilities
  -- *** 'initialized'
  , pattern Initialized
  -- *** 'exit'
  , pattern Exit
  -- *** 'shutdown'
  , pattern Shutdown
  -- *** 'window/logMessage'
  , pattern LogMessage
  , LogMessageParams(..)
  -- *** 'window/showMessage'
  , pattern ShowMessage
  , ShowMessageParams(..)
  -- *** 'telemetry/event'
  , pattern TelemetryEvent
  -- *** 'client/registerCapability'
  , pattern RegisterCapability
  , Registration(..)
  , TextDocumentRegistrationOptions(..)
  , pattern RegisterCapabilityResponse
  -- *** 'client/unregisterCapability'
  , pattern UnregisterCapability
  , Unregistration(..)
  -- *** 'workspace/didChangeConfiguration'
  , pattern DidChangeConfiguration
  , DidChangeConfigurationParams(..)
  -- *** 'textDocument/didOpen'
  , pattern DidOpen
  , DidOpenTextDocumentParams(..)
  -- *** 'textDocument/didChange'
  , pattern DidChange
  , DidChangeTextDocumentParams(..)
  , TextDocumentContentChangeEvent(..)
  -- *** 'textDocument/didClose'
  , pattern DidClose
  , DidCloseTextDocumentParams(..)
  -- *** 'workspace/didChangeWatchedFiles
  , pattern DidChangeWatchedFiles
  , DidChangeWatchedFilesParams(..)
  , FileEvent(..)
  -- *** 'textDocument/publishDiagnostics'
  , pattern PublishDiagnostics
  , PublishDiagnosticsParams(..)
  -- *** 'textDocument/hover'
  , pattern Hover
  , HoverResult(..)
  , MarkedString(..)
  -- * Ad-hoc Overloading
  , HasArguments(..)
  , HasCapabilities(..)
  , HasChanges(..)
  , HasCharacter(..)
  , HasCode(..)
  , HasCommand(..)
  , HasContentChanges(..)
  , HasContents(..)
  , HasData(..)
  , HasDiagnostics(..)
  , HasDocumentChanges(..)
  , HasDocumentSelector(..)
  , HasEdits(..)
  , HasEnd(..)
  , HasError(..)
  , HasExperiment(..)
  , HasId(..)
  , HasInitializationOptions(..)
  , HasLanguage(..)
  , HasLanguageId(..)
  , HasLine(..)
  , HasMessage(..)
  , HasMethod(..)
  , HasNewText(..)
  , HasParams(..)
  , HasPattern(..)
  , HasPosition(..)
  , HasProcessId(..)
  , HasRange(..)
  , HasRangeLength(..)
  , HasRegisterOptions(..)
  , HasResult(..)
  , HasRootPath(..)
  , HasRootUri(..)
  , HasScheme(..)
  , HasSettings(..)
  , HasSeverity(..)
  , HasSource(..)
  , HasStart(..)
  , HasText(..)
  , HasTextDocument(..)
  , HasTitle(..)
  , HasTrace(..)
  , HasType(..)
  , HasUri(..)
  , HasValue(..)
  , HasVersion(..)
  , HasWorkspace(..)
  ) where

import Coda.Util.Aeson
import Control.Applicative
import Control.Lens.TH
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Aeson.Encoding
import Data.Aeson.Internal
import Data.Data
import Data.Default
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Ix (Ix)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.String
import Data.Text as Text
import GHC.Generics
import Language.Server.TH
import Network.URI.Encode as URI
import Text.Read as Read hiding (Number, String)

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

makeWrapped ''ErrorCode

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

--------------------------------------------------------------------------------
-- Cancellation Notification
--------------------------------------------------------------------------------

-- | Note: You should still reply to the item in question
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#-cancellation-support>

pattern CancelRequest :: Id -> Request
pattern CancelRequest i = Request Nothing "$/cancelRequest" (Just (JSON i))

pattern RequestCancelled :: ErrorCode
pattern RequestCancelled = ErrorCode (-32800)

-- We don't reply to the CancelRequest, as it is a notification, but we should
-- cause the computation that was cancelled to respond with something like this:
cancelledResponse :: Id -> Response
cancelledResponse i = Response (Just i) Nothing (Just (ResponseError RequestCancelled "request cancelled" Nothing))

--------------------------------------------------------------------------------
-- DocumentUri
--------------------------------------------------------------------------------

-- |
-- | <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#uri>

newtype DocumentUri = DocumentUri Text
  deriving (Eq,Ord,Show,Read,Data,Generic)

makeWrapped ''DocumentUri

instance ToJSON DocumentUri where
  toJSON (DocumentUri t) = toJSON t
  toEncoding (DocumentUri t) = toEncoding t

instance FromJSON DocumentUri where
  parseJSON v = DocumentUri <$> parseJSON v

instance Hashable DocumentUri

instance IsString DocumentUri where
  fromString = DocumentUri . fromString

instance Default DocumentUri where
  def = "file:///"

-- | Encode/decode FilePaths <-> "file://" urls
pattern File :: FilePath -> DocumentUri
pattern File path <- (documentUriToFilePath -> Just path) where
  File path = DocumentUri $ Text.pack $ "file://" ++ URI.encode path

documentUriToFilePath :: DocumentUri -> Maybe FilePath
documentUriToFilePath (DocumentUri u)
  = adjust . URI.decode . Text.unpack <$> Text.stripPrefix "file://" u
  where
    adjust ('/':xs@(_:':':_)) = xs
    adjust xs = xs

--------------------------------------------------------------------------------
-- Position
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#position>
--
-- @
-- interface 'Position' {
--   line: number;
--   character: number;
-- }
-- @

data Position = Position
  { _line :: !Int -- ^ 0-based line number
  , _character :: !Int -- ^ 0-based count of utf-16 words (not code-points!)
  } deriving (Eq, Ord, Show, Read, Data, Generic)

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position a b) (Position 0 d) = Position a (b + d)
  mappend (Position a _) (Position c d) = Position (a + c) d

jsonKeep ''Position
lenses ''Position
instance Hashable Position
instance Default Position

-- |
-- @'subtractPosition' a b@ subtracts @a@ from @b@. 
-- 
-- Be careful, as this is the opposite argument order from @(-)@, but matches
-- the argument order of 'subtract'
--
-- This is a clamped subtraction, much as if we defined subtraction on natural
-- numbers to be total by defining
-- @n - m | m >= n = 0@
--
-- Because of the necessary clamping, Position does not form a group.
subtractPosition :: Position -> Position -> Position
subtractPosition (Position l1 c1) (Position l2 c2) = case compare l1 l2 of
  LT -> Position (l2-l1) c2
  EQ -> Position 0 $ max (c2-c1) 0
  GT -> Position 0 0

--------------------------------------------------------------------------------
-- Range
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#range>
--
-- @
-- interface 'Range' {
--   start: 'Position';
--   end: 'Position';
-- }
-- @
data Range = Range
  { _start :: !Position
  , _end   :: !Position
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonKeep ''Range
lenses ''Range
instance Hashable Range
instance Default Range

-- | Compute the relative position of the end of the range from the start of the range
rangeSize :: Range -> Position
rangeSize (Range lo hi) = subtractPosition lo hi

--------------------------------------------------------------------------------
-- Location
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#location>
--
-- @
-- interface 'Location' {
--   uri: 'DocumentUri';
--   range: 'Range';
-- }
-- @
data Location = Location
  { _uri   :: DocumentUri
  , _range :: Range
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonKeep ''Location
lenses ''Location
instance Hashable Location
instance Default Location

--------------------------------------------------------------------------------
-- Diagnostic
--------------------------------------------------------------------------------

-- |
-- See @DiagnosticSeverity@ in
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#diagnostic>
newtype Severity = Severity Int
  deriving (Eq,Ord,Enum,Bounded,Ix,Data,Generic)

makeWrapped ''Severity

instance ToJSON Severity
instance FromJSON Severity
instance Hashable Severity
instance Default Severity where
  def = Information

-- | Reports an error.
pattern Error :: Severity
pattern Error = Severity 1

-- | Reports a warning.
pattern Warning :: Severity
pattern Warning = Severity 2

-- | Reports information
pattern Information :: Severity
pattern Information = Severity 3

-- | Reports a hint
pattern Hint :: Severity
pattern Hint = Severity 4

instance Show Severity where
  showsPrec d = \case
    Error       -> showString "Error"
    Warning     -> showString "Warning"
    Information -> showString "Information"
    Hint        -> showString "Hint"
    Severity n  -> showParen (d > 10) $ showString "Severity " . showsPrec 10 n

instance Read Severity where
  readPrec = Read.parens
      $ do Read.Ident "Error" <- Read.lexP
           return Error
    <|> do Read.Ident "Warning" <- Read.lexP
           return Warning
    <|> do Read.Ident "Information" <- Read.lexP
           return Information
    <|> do Read.Ident "Hint" <- Read.lexP
           return Hint
    <|> do Read.prec 10 $ do
             Read.Ident "Severity" <- lexP
             Severity <$> step readPrec

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#diagnostic>
--
-- @
-- interface 'Diagnostic' {
--   range: 'Range';
--   severity?: number;
--   code?: number | string;
--   source?: string;
--   message: string;
-- }
-- @
data Diagnostic = Diagnostic
  { _range    :: !Range
  , _severity :: !(Maybe Severity)
  , _code     :: !(Maybe Id)
  , _source   :: !(Maybe Text)
  , _message  :: !Text
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonOmit ''Diagnostic
lenses ''Diagnostic
instance Hashable Diagnostic
instance Default Diagnostic where
  def = Diagnostic def def def def ""

--------------------------------------------------------------------------------
-- Command
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#command>
--
-- @
-- interface 'Command' {
--   title : string;
--   command: string;
--   arguments?: any[]
-- }
-- @

data Command = Command
  { _title  :: Text
  , _command :: Text
  , _arguments :: Maybe [Value]
  } deriving (Eq, Show, Read, Data, Generic)

jsonOmit ''Command
lenses ''Command
instance Hashable Command
instance Default Command where
  def = Command "" "" def

--------------------------------------------------------------------------------
-- TextEdit
--------------------------------------------------------------------------------

-- | <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textedit>
--
-- @
-- interface 'TextEdit' {
--   range: 'Range';
--   newText: string;
-- }
-- @
data TextEdit = TextEdit
  { _range   :: !Range
  , _newText :: !Text
  } deriving (Eq,Ord,Show,Read,Data,Generic)

jsonKeep ''TextEdit
lenses ''TextEdit
instance Hashable TextEdit
instance Default TextEdit where
  def = TextEdit def ""

--------------------------------------------------------------------------------
-- TextDocumentIdentifier
--------------------------------------------------------------------------------

-- | <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentidentifier>
--
-- @
-- interface 'TextDocumentIdentifier' {
--   uri : 'DocumentUri'
-- }
-- @
newtype TextDocumentIdentifier = TextDocumentIdentifier
  { _uri :: DocumentUri
  } deriving (Eq, Ord, Show, Read, Data, Generic)

makeWrapped ''TextDocumentIdentifier
jsonKeep ''TextDocumentIdentifier
lenses ''TextDocumentIdentifier
instance Hashable TextDocumentIdentifier
instance Default TextDocumentIdentifier

--------------------------------------------------------------------------------
-- VersionTextDocumentIdentifier
--------------------------------------------------------------------------------

-- | <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#versionedtextdocumentidentifier>
--
-- @
-- interface 'VersionedTextDocumentIdentifier' extends 'TextDocumentIdentifier' {
--   version: number;
-- }
-- @
data VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  { _uri     :: !DocumentUri
  , _version :: !Int
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonKeep ''VersionedTextDocumentIdentifier
lenses ''VersionedTextDocumentIdentifier
instance Hashable VersionedTextDocumentIdentifier
instance Default VersionedTextDocumentIdentifier

--------------------------------------------------------------------------------
-- TextDocumentEdit
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#new-textdocumentedit>
--
-- @
-- export interface 'TextDocumentEdit' {
--   textDocument: 'VersionedTextDocumentIdentifier';
--   edits: 'TextEdit'[];
-- }
-- @
data TextDocumentEdit = TextDocumentEdit
  { _textDocument :: !VersionedTextDocumentIdentifier
  , _edits        :: [TextEdit]
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonKeep ''TextDocumentEdit
lenses ''TextDocumentEdit
instance Hashable TextDocumentEdit
instance Default TextDocumentEdit

--------------------------------------------------------------------------------
-- WorkspaceEdit
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#workspaceedit>
--
-- @
-- export interface 'WorkspaceEdit' {
--   changes?: { [uri: string]: 'TextEdit'[]; };
--   documentChanges?: 'TextDocumentEdit'[];
-- }
-- @

data WorkspaceEdit = WorkspaceEdit
  { _changes         :: !(Maybe (HashMap Text [TextEdit]))
  , _documentChanges :: !(Maybe [TextDocumentEdit])
  } deriving (Eq, Show, Read, Data, Generic)

jsonOmit ''WorkspaceEdit
lenses ''WorkspaceEdit
instance Hashable WorkspaceEdit
instance Default WorkspaceEdit

--------------------------------------------------------------------------------
-- TextDocumentItem
--------------------------------------------------------------------------------

-- | An item to transfer a text document from the client to the server.
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentitem>
--
-- @
-- interface TextDocumentItem {
--   uri: DocumentUri;
--   languageId: string;
--   version: number;
--   text: string;
-- }
-- @
data TextDocumentItem = TextDocumentItem
  { _uri        :: !DocumentUri
  , _languageId :: !Text
  , _version    :: !Int
  , _text       :: !Text
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonKeep ''TextDocumentItem
lenses ''TextDocumentItem
instance Hashable TextDocumentItem
instance Default TextDocumentItem where
  def = TextDocumentItem def "" def ""

--------------------------------------------------------------------------------
-- DocumentFilter
--------------------------------------------------------------------------------

-- |
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#documentfilter>
--
-- @
-- export interface 'DocumentFilter' {
--	language?: string;
--	scheme?: string;
--	pattern?: string;
-- }
-- @
data DocumentFilter = DocumentFilter
  { _language :: Maybe String
  , _scheme   :: Maybe String
  , _pattern  :: Maybe String
  } deriving (Eq, Ord, Show, Read, Data, Generic)

jsonOmit ''DocumentFilter
lenses ''DocumentFilter
instance Hashable DocumentFilter
instance Default DocumentFilter

-- type DocumentSelector = [DocumentFilter]

--------------------------------------------------------------------------------
-- TextDocumentPositionParams
--------------------------------------------------------------------------------

-- |
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentpositionparams>
--
data TextDocumentPositionParams = TextDocumentPositionParams
  { _textDocument :: TextDocumentIdentifier
  , _position     :: Position
  } deriving (Eq, Ord, Show, Read, Data, Generic)

type TDPP = TextDocumentPositionParams

lenses ''TextDocumentPositionParams
jsonKeep ''TextDocumentPositionParams
instance Hashable TextDocumentPositionParams
instance Default TextDocumentPositionParams

--------------------------------------------------------------------------------
-- Trace
--------------------------------------------------------------------------------

data Trace
  = TraceOff
  | TraceMessages
  | TraceVerbose
  deriving (Eq,Ord,Show,Read,Data,Generic,Ix,Bounded,Enum)

instance ToJSON Trace where
  toJSON TraceOff = String "off"
  toJSON TraceMessages = String "messages"
  toJSON TraceVerbose = String "verbose"

instance FromJSON Trace where
  parseJSON = withText "Trace" $ \case
    "off"      -> pure TraceOff
    "messages" -> pure TraceMessages
    "verbose"  -> pure TraceVerbose
    _ -> mzero

instance Hashable Trace

instance Default Trace where
  def = TraceOff

--------------------------------------------------------------------------------
-- Client -> Server: 'initialize'
--------------------------------------------------------------------------------

type WorkspaceClientCapabilities = Value
type TextDocumentClientCapabilities = Value

data ClientCapabilities = ClientCapabilities
  { _workspace    :: Maybe WorkspaceClientCapabilities
  , _textDocument :: Maybe TextDocumentClientCapabilities
  , _experiment   :: Maybe Value
  } deriving (Eq,Show,Read,Data,Generic)

jsonOmit ''ClientCapabilities
lenses ''ClientCapabilities
instance Hashable ClientCapabilities
instance Default ClientCapabilities

data InitializeParams = InitializeParams
  { _processId             :: Maybe Int
  , _rootPath              :: Maybe (Maybe String)
  , _rootUri               :: Maybe DocumentUri
  , _initializationOptions :: Maybe Value
  , _capabilities          :: ClientCapabilities
  , _trace                 :: Trace
  } deriving (Eq,Show,Read,Data,Generic)

instance Default InitializeParams

instance ToJSON InitializeParams where
  toJSON (InitializeParams p rp ru o c t) = object $
     [ "processId"    .= p
     , "rootUri"      .= ru
     , "capabilities" .= c
     , "trace"        .= t
     ] ++ catMaybes
     [ ("rootPath" .=) <$> rp
     , ("initializationOptions" .=) <$> o
     ]

instance FromJSON InitializeParams where
  parseJSON = withObject "InitializeParams" $ \v -> InitializeParams
    <$> v .: "processId"
    <*> v .:? "rootPath"
    <*> v .: "rootUri"
    <*> v .:? "initializationOptions"
    <*> v .: "capabilities"
    <*> v .: "trace" .!= TraceOff

lenses ''InitializeParams
instance Hashable InitializeParams

-- | @initialize@
pattern Initialize :: Id -> InitializeParams -> Request
pattern Initialize i p = Request (Just i) "initialize" (Just (JSON p))

--------------------------------------------------------------------------------
-- Client -> Server: 'initialized'
--------------------------------------------------------------------------------

-- | @initialized@
pattern Initialized :: Request
pattern Initialized = Request Nothing "initialized" Nothing

--------------------------------------------------------------------------------
-- Client -> Server: 'shutdown'
--------------------------------------------------------------------------------

-- | @shutdown@
pattern Shutdown :: Request
pattern Shutdown = Request Nothing "shutdown" Nothing

--------------------------------------------------------------------------------
-- Client -> Server: 'exit'
--------------------------------------------------------------------------------

-- | @exit@
pattern Exit :: Request
pattern Exit = Request Nothing "exit" Nothing

--------------------------------------------------------------------------------
-- Server -> Client: 'window/logMessage'
--------------------------------------------------------------------------------

data LogMessageParams = LogMessageParams
  { _type    :: !Severity
  , _message :: !Text
  } deriving (Eq,Show,Read,Data,Generic)

instance Default LogMessageParams where
  def = LogMessageParams def ""

jsonOmit ''LogMessageParams
lenses ''LogMessageParams
instance Hashable LogMessageParams

-- | @window/logMessage@
pattern LogMessage :: Severity -> Text -> Request
pattern LogMessage s m = Request Nothing "window/logMessage" (Just (JSON (LogMessageParams s m)))

--------------------------------------------------------------------------------
-- Server -> Client: 'window/showMessage'
--------------------------------------------------------------------------------

data ShowMessageParams = ShowMessageParams
  { _type    :: !Severity
  , _message :: !Text
  } deriving (Eq,Show,Read,Data,Generic)

instance Default ShowMessageParams where
  def = ShowMessageParams def ""

jsonOmit ''ShowMessageParams
lenses ''ShowMessageParams
instance Hashable ShowMessageParams

-- | @window/showMessage@
pattern ShowMessage :: Severity -> Text -> Request
pattern ShowMessage s m = Request Nothing "window/showMessage" (Just (JSON (ShowMessageParams s m)))

--------------------------------------------------------------------------------
-- Server -> Client: 'telemetry/event'
--------------------------------------------------------------------------------

-- | @telemetry/event@
pattern TelemetryEvent :: Value -> Request
pattern TelemetryEvent v = Request Nothing "telemetry/event" (Just v)

--------------------------------------------------------------------------------
-- Server -> Client: 'client/registerCapability'
--------------------------------------------------------------------------------

data Registration = Registration
  { _id :: !Text
  , _method :: !Text
  , _registerOptions :: !(Maybe Value)
  } deriving (Eq,Show,Read,Data,Generic)

jsonOmit ''Registration
lenses ''Registration
instance Hashable Registration

instance Default Registration where
  def = Registration "" "" def

newtype TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  { _documentSelector :: Maybe [DocumentFilter]
  } deriving (Eq,Show,Read,Data,Generic)

instance Default TextDocumentRegistrationOptions

jsonKeep ''TextDocumentRegistrationOptions
lenses ''TextDocumentRegistrationOptions
instance Hashable TextDocumentRegistrationOptions

-- | @client/registerCapability@
pattern RegisterCapability :: Id -> [Registration] -> Request
pattern RegisterCapability i rs = Request (Just i) "client/registerCapability" (Just (JSON rs))

pattern RegisterCapabilityResponse :: Id -> Maybe ResponseError -> Response
pattern RegisterCapabilityResponse i e = Response (Just i) Nothing e

--------------------------------------------------------------------------------
-- Server -> Client: 'client/unregisterCapability'
--------------------------------------------------------------------------------

data Unregistration = Unregistration
  { _id :: !Text
  , _method :: !Text
  } deriving (Eq,Show,Read,Data,Generic)

instance Default Unregistration where
  def = Unregistration "" ""

jsonKeep ''Unregistration
lenses ''Unregistration
instance Hashable Unregistration

-- | @client/unregisterCapability@
pattern UnregisterCapability :: Id -> [Unregistration] -> Request
pattern UnregisterCapability i urs = Request (Just i) "client/unregisterCapability" (Just (JSON urs))

--------------------------------------------------------------------------------
-- Client -> Server: 'workspace/didChangeConfiguration'
--------------------------------------------------------------------------------

newtype DidChangeConfigurationParams = DidChangeConfigurationParams
  { _settings :: Value
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidChangeConfigurationParams
lenses ''DidChangeConfigurationParams
makeWrapped ''DidChangeConfigurationParams
instance Hashable DidChangeConfigurationParams

instance Default DidChangeConfigurationParams where
  def = DidChangeConfigurationParams Null

-- | @workspace/didChangeConfiguration@
pattern DidChangeConfiguration :: Value -> Request
pattern DidChangeConfiguration v = Request Nothing "workspace/didChangeConfiguration" (Just (JSON (DidChangeConfigurationParams v)))

--------------------------------------------------------------------------------
-- Client -> Server: 'textDocument/didOpen'
--------------------------------------------------------------------------------

newtype DidOpenTextDocumentParams = DidOpenTextDocumentParams
  { _textDocument :: TextDocumentItem
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidOpenTextDocumentParams
lenses ''DidOpenTextDocumentParams
makeWrapped ''DidOpenTextDocumentParams
instance Hashable DidOpenTextDocumentParams
instance Default DidOpenTextDocumentParams

-- | @textDocument/didOpen@
pattern DidOpen :: TextDocumentItem -> Request
pattern DidOpen tdi = Request Nothing "textDocument/didOpen" (Just (JSON (DidOpenTextDocumentParams tdi)))

--------------------------------------------------------------------------------
-- Client -> Server: 'textDocument/didClose'
--------------------------------------------------------------------------------

newtype DidCloseTextDocumentParams = DidCloseTextDocumentParams
  { _textDocument :: TextDocumentItem
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidCloseTextDocumentParams
lenses ''DidCloseTextDocumentParams
makeWrapped ''DidCloseTextDocumentParams
instance Hashable DidCloseTextDocumentParams
instance Default DidCloseTextDocumentParams

-- | @textDocument/didClose@
pattern DidClose :: TextDocumentItem -> Request
pattern DidClose tdi = Request Nothing "textDocument/didClose" (Just (JSON (DidCloseTextDocumentParams tdi)))

--------------------------------------------------------------------------------
-- Client -> Server: 'textDocument/didChange'
--------------------------------------------------------------------------------

data TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  { _range :: !(Maybe Range)
  , _rangeLength :: !(Maybe Int)
  , _text :: !Text
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''TextDocumentContentChangeEvent
lenses ''TextDocumentContentChangeEvent
instance Hashable TextDocumentContentChangeEvent

instance Default TextDocumentContentChangeEvent where
  def = TextDocumentContentChangeEvent def def ""

data DidChangeTextDocumentParams = DidChangeTextDocumentParams
  { _textDocument :: !VersionedTextDocumentIdentifier
  , _contentChanges :: [TextDocumentContentChangeEvent]
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidChangeTextDocumentParams
lenses ''DidChangeTextDocumentParams
instance Hashable DidChangeTextDocumentParams

instance Default DidChangeTextDocumentParams

-- | @textDocument/didChange@
pattern DidChange :: DidChangeTextDocumentParams -> Request
pattern DidChange p = Request Nothing "textDocument/didChange" (Just (JSON p))



--------------------------------------------------------------------------------
-- Client -> Server: 'workspace/didChangeWatchedFiles'
--------------------------------------------------------------------------------

data FileChangeType
  = Created | Changed | Deleted
  deriving (Eq,Ord,Show,Read,Data,Typeable,Enum,Ix,Bounded,Generic)

instance ToJSON FileChangeType where
  toJSON Created = Number 1
  toJSON Changed = Number 2
  toJSON Deleted = Number 3

instance FromJSON FileChangeType where
  parseJSON (Number 1) = pure Created
  parseJSON (Number 2) = pure Changed
  parseJSON (Number 3) = pure Deleted
  parseJSON _ = fail "expected FileChangeType"

instance Hashable FileChangeType

instance Default FileChangeType where
  def = Created

data FileEvent = FileEvent
  { _uri :: !DocumentUri
  , _type :: !FileChangeType
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''FileEvent
lenses ''FileEvent
instance Hashable FileEvent
instance Default FileEvent

newtype DidChangeWatchedFilesParams = DidChangeWatchedFilesParams
  { _changes :: [FileEvent]
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidChangeWatchedFilesParams
lenses ''DidChangeWatchedFilesParams
instance Hashable DidChangeWatchedFilesParams
instance Default DidChangeWatchedFilesParams

-- | @workspace/didChangeWatchedFiles@
pattern DidChangeWatchedFiles :: [FileEvent] -> Request
pattern DidChangeWatchedFiles c = Request Nothing "workspace/didChangeWatchedFiles" (Just (JSON (DidChangeWatchedFilesParams c)))


--------------------------------------------------------------------------------
-- Server -> Client: 'textDocument/publishDiagnostics'
--------------------------------------------------------------------------------

data PublishDiagnosticsParams = PublishDiagnosticsParams
  { _uri :: !DocumentUri
  , _diagnostics :: [Diagnostic]
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''PublishDiagnosticsParams
lenses ''PublishDiagnosticsParams
instance Hashable PublishDiagnosticsParams
instance Default PublishDiagnosticsParams

-- | @textDocument/publishDiagnostics@
pattern PublishDiagnostics :: PublishDiagnosticsParams -> Request
pattern PublishDiagnostics p = Request Nothing "textDocument/publishDiagnostics" (Just (JSON p))

--------------------------------------------------------------------------------
-- Server -> Client: 'textDocument/hover'
--------------------------------------------------------------------------------

pattern Hover :: Id -> TextDocumentPositionParams -> Request
pattern Hover i p = Request (Just i) "textDocument/hover" (Just (JSON p))

data MarkedString = MarkedString { _language :: !(Maybe Text), _value :: !Text }
  deriving (Eq,Show,Read,Data,Generic)

lenses ''MarkedString

instance ToJSON MarkedString where
  toJSON (MarkedString Nothing v)  = toJSON v
  toJSON (MarkedString (Just l) v) = object [ "language" .= l, "value" .= v ]

instance FromJSON MarkedString where
  parseJSON v = withText "MarkedString" (pure . MarkedString Nothing) v
            <|> withObject "MarkedString" (\m -> MarkedString <$> m .: "language" <*> m .: "value") v
instance Hashable MarkedString

instance IsString MarkedString where
  fromString = MarkedString Nothing . fromString

-- @
-- interface Hover {
--   contents : MarkedString | MarkedString[]
--   range?: Range;
-- @
data HoverResult = HoverResult
  { _contents :: [MarkedString]
  , _range    :: Maybe Range
  } deriving (Eq,Show,Read,Data,Generic)

lenses ''HoverResult

instance ToJSON HoverResult where
  toJSON (HoverResult xs r) = object
    $ case xs of
        [x] -> "contents" .= x
        _   -> "contents" .= xs
    : toList (("range" .=) <$> r)

instance FromJSON HoverResult where
  parseJSON = withObject "Hover" $ \v -> HoverResult
    <$> (pure <$> v .: "contents" <|> v .: "contents")
    <*> v .:? "range"

instance Hashable HoverResult
