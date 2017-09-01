{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language DeriveTraversable #-}
{-# language DeriveDataTypeable #-}
{-# language DuplicateRecordFields #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeSynonymInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md Language Server Protocol>
--------------------------------------------------------------------------------

module Language.Server.Protocol
  (
  -- * Cancellation Support
    pattern CancelRequest
  , pattern RequestCancelled
  , cancelledResponse
  -- * Language Server Protocol
  , Severity(..)
  , DocumentUri
  , Position(..)
  , Range(..)
  , Location(..)
  , Diagnostic(..)
  , Command(..)
  , TextEdit(..)
  , TextDocumentIdentifier(..)
  , VersionedTextDocumentIdentifier(..)
  , TextDocumentItem(..)
  , TextDocumentEdit(..)
  , WorkspaceEdit(..)
  , DocumentFilter(..)
  , DocumentSelector
  , TextDocumentPositionParams(..), TDPP
  -- * Protocol
  -- ** 'initialize'
  , pattern Initialize
  , InitializeParams(..)
  , ClientCapabilities(..)
  , WorkspaceClientCapabilities
  , TextDocumentClientCapabilities
  -- ** 'initialized'
  , pattern Initialized
  -- ** 'exit'
  , pattern Exit
  -- ** 'shutdown'
  , pattern Shutdown
  -- ** 'window/logMessage'
  , pattern LogMessage
  , LogMessageParams(..)
  -- ** 'window/showMessage'
  , pattern ShowMessage
  , ShowMessageParams(..)
  -- ** 'telemetry/event'
  , pattern TelemetryEvent
  -- ** 'client/registerCapability'
  , pattern RegisterCapability
  , Registration(..)
  , TextDocumentRegistrationOptions(..)
  , pattern RegisterCapabilityResponse
  -- ** 'client/unregisterCapability'
  , pattern UnregisterCapability
  , Unregistration(..)
  -- ** 'workspace/didChangeConfiguration'
  , pattern DidChangeConfiguration
  , DidChangeConfigurationParams(..)
  -- ** 'textDocument/didOpen'
  , pattern DidOpen
  , DidOpenTextDocumentParams(..)
  -- ** 'textDocument/didChange'
  , pattern DidChange
  , DidChangeTextDocumentParams(..)
  , TextDocumentContentChangeEvent(..)
  -- ** 'workspace/didChangeWatchedFiles
  , pattern DidChangeWatchedFiles
  , DidChangeWatchedFilesParams(..)
  , FileEvent(..)
  -- ** 'textDocument/publishDiagnostics'
  , pattern PublishDiagnostics
  , PublishDiagnosticsParams(..)
  -- * Ad-hoc Overloading
  , HasArguments(..)
  , HasCapabilities(..)
  , HasChanges(..)
  , HasCharacter(..)
  , HasCode(..)
  , HasCommand(..)
  , HasContentChanges(..)
  , HasDiagnostics(..)
  , HasDocumentChanges(..)
  , HasDocumentSelector(..)
  , HasEdits(..)
  , HasEnd(..)
  , HasExperiment(..)
  , HasId(..)
  , HasInitializationOptions(..)
  , HasLanguage(..)
  , HasLanguageId(..)
  , HasLine(..)
  , HasMethod(..)
  , HasMessage(..)
  , HasNewText(..)
  , HasPattern(..)
  , HasPosition(..)
  , HasProcessId(..)
  , HasRange(..)
  , HasRangeLength(..)
  , HasRegisterOptions(..)
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
  , HasVersion(..)
  , HasWorkspace(..)
  ) where

import Coda.Util.Aeson
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Data
import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.Ix (Ix)
import Data.Maybe (catMaybes)
import Data.Text as Text
import GHC.Generics
import Language.Server.Base
import Language.Server.Severity as Severity
import Language.Server.TH

--------------------------------------------------------------------------------
-- Cancellation Notification
--------------------------------------------------------------------------------

-- | Note: You should still reply to the item in question
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#-cancellation-support>

pattern CancelRequest :: Id -> Request
pattern CancelRequest identifier = Request Nothing "$/cancelRequest" (Just (Value_ identifier))

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

type DocumentUri = Text

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

jsonKeep ''Position
lenses ''Position
instance Hashable Position

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

--------------------------------------------------------------------------------
-- Diagnostic
--------------------------------------------------------------------------------

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

jsonKeep ''TextDocumentIdentifier
lenses ''TextDocumentIdentifier
instance Hashable TextDocumentIdentifier

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

--------------------------------------------------------------------------------
-- DocumentSelector
--------------------------------------------------------------------------------

type DocumentSelector = [DocumentFilter]

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


data InitializeParams = InitializeParams
  { _processId             :: Maybe Int
  , _rootPath              :: Maybe (Maybe String)
  , _rootUri               :: Maybe DocumentUri
  , _initializationOptions :: Maybe Value
  , _capabilities          :: ClientCapabilities
  , _trace                 :: Trace
  } deriving (Eq,Show,Read,Data,Generic)

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

data TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  { _documentSelector :: Maybe DocumentSelector
  } deriving (Eq,Show,Read,Data,Generic)

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
instance Hashable DidChangeConfigurationParams

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
instance Hashable DidOpenTextDocumentParams

-- | @textDocument/didOpen@
pattern DidOpen :: TextDocumentItem -> Request
pattern DidOpen tdi = Request Nothing "textDocument/didOpen" (Just (JSON (DidOpenTextDocumentParams tdi)))

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

data DidChangeTextDocumentParams = DidChangeTextDocumentParams
  { _textDocument :: !VersionedTextDocumentIdentifier
  , _contentChanges :: [TextDocumentContentChangeEvent]
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidChangeTextDocumentParams
lenses ''DidChangeTextDocumentParams
instance Hashable DidChangeTextDocumentParams

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

data FileEvent = FileEvent
  { _uri :: !DocumentUri
  , _type :: !FileChangeType
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''FileEvent
lenses ''FileEvent
instance Hashable FileEvent

data DidChangeWatchedFilesParams = DidChangeWatchedFilesParams
  { _changes :: [FileEvent]
  } deriving (Eq,Show,Read,Data,Generic)

jsonKeep ''DidChangeWatchedFilesParams
lenses ''DidChangeWatchedFilesParams
instance Hashable DidChangeWatchedFilesParams

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

-- | @textDocument/publishDiagnostics@
pattern PublishDiagnostics :: PublishDiagnosticsParams -> Request
pattern PublishDiagnostics p = Request Nothing "textDocument/publishDiagnostics" (Just (JSON p))
