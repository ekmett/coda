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
  , WorkspaceClientCapabilities
  -- * Protocol
  , TextDocumentClientCapabilities
  , ClientCapabilities(..)
  , InitializeParams(..)
  -- * Overloading
  , HasPosition(..)
  , HasRange(..)
  , HasUri(..)
  , HasVersion(..)
  , HasLine(..)
  , HasCharacter(..)
  , HasEnd(..)
  , HasStart(..) 
  , HasMessage(..)
  , HasCode(..)
  , HasSeverity(..)
  , HasSource(..)
  , HasCommand(..)
  , HasArguments(..)
  , HasTitle(..)
  , HasNewText(..)
  , HasTextDocument(..)
  , HasEdits(..)
  , HasChanges(..)
  , HasDocumentChanges(..)
  , HasText(..)
  , HasLanguageId(..)
  , HasPattern(..)
  , HasLanguage(..)
  , HasScheme(..)
  , HasExperiment(..)
  , HasWorkspace(..)
  , HasTrace(..) 
  , HasProcessId(..)
  , HasRootPath(..)
  , HasRootUri(..)
  , HasInitializationOptions(..)
  , HasCapabilities(..)
  ) where

import Coda.Util.Aeson
import Control.Lens.Combinators
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import Data.Data
import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.Ix (Ix)
import Data.Maybe (catMaybes)
import Data.Text as Text
import GHC.Generics
import Language.Server.Base
import Language.Server.Severity as Severity

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

deriveJSON keepOptions ''Position
makeFieldsNoPrefix ''Position
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

deriveJSON keepOptions ''Range
makeFieldsNoPrefix ''Range
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

deriveJSON keepOptions ''Location
makeFieldsNoPrefix ''Location
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

deriveJSON omitOptions ''Diagnostic
makeFieldsNoPrefix ''Diagnostic
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

deriveJSON omitOptions ''Command
makeFieldsNoPrefix ''Command
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

deriveJSON keepOptions ''TextEdit
makeFieldsNoPrefix ''TextEdit
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

deriveJSON keepOptions ''TextDocumentIdentifier
makeFieldsNoPrefix ''TextDocumentIdentifier
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

deriveJSON keepOptions ''VersionedTextDocumentIdentifier
makeFieldsNoPrefix ''VersionedTextDocumentIdentifier
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

deriveJSON keepOptions ''TextDocumentEdit
makeFieldsNoPrefix ''TextDocumentEdit
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

deriveJSON omitOptions ''WorkspaceEdit
makeFieldsNoPrefix ''WorkspaceEdit
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

deriveJSON keepOptions ''TextDocumentItem
makeFieldsNoPrefix ''TextDocumentItem
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

deriveJSON omitOptions ''DocumentFilter
makeFieldsNoPrefix ''DocumentFilter
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

makeFieldsNoPrefix ''TextDocumentPositionParams
deriveJSON keepOptions ''TextDocumentPositionParams
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
-- Boilerplate
--------------------------------------------------------------------------------

type WorkspaceClientCapabilities = Value
type TextDocumentClientCapabilities = Value

--------------------------------------------------------------------------------
-- ClientCapabilities
--------------------------------------------------------------------------------

data ClientCapabilities = ClientCapabilities
  { _workspace    :: Maybe WorkspaceClientCapabilities
  , _textDocument :: Maybe TextDocumentClientCapabilities
  , _experiment   :: Maybe Value
  } deriving (Eq,Show,Read,Data,Generic)

deriveJSON omitOptions ''ClientCapabilities
makeFieldsNoPrefix ''ClientCapabilities
instance Hashable ClientCapabilities

--------------------------------------------------------------------------------
-- InitializeParams
--------------------------------------------------------------------------------

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

makeFieldsNoPrefix ''InitializeParams

instance Hashable InitializeParams
