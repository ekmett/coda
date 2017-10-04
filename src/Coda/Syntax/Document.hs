{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Coda.Syntax.Document
  ( Document(..)
  , HasLanguageId(..)
  , HasVersion(..)
  , HasContents(..)
  , didOpen
  , didChange
  , didClose
  -- , didChangeWatchedFiles
  ) where

import Coda.Syntax.Rope
import Control.Lens
import Control.Monad.State
import Data.Function (on)
import Data.HashMap.Strict hiding (foldr)
import Data.List (sortBy)
import Data.Text as Text hiding (lines, foldr)
import Language.Server.Protocol
import Prelude hiding (lines)

data Document = Document
  { _languageId :: !Text
  , _version    :: {-# unpack #-} !Int
  , _contents      :: !Rope
  } deriving Show

makeFieldsNoPrefix ''Document

type Documents = HashMap DocumentUri Document

class HasDocuments t where
  documents :: Lens' t Documents

didOpen :: (MonadState s m, HasDocuments s) => TextDocumentItem -> m ()
didOpen (TextDocumentItem u l v t) = documents.at u ?= Document l v (fromText t)

didChange :: (MonadState s m, HasDocuments s) => DidChangeTextDocumentParams -> m ()
didChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier u v) cs)
  = modifying (documents.ix u) $ execState $ do
    version .= v
    modifying contents $ foldr apply ?? sortBy (compare `on` view range) cs
  where
    apply :: TextDocumentContentChangeEvent -> Rope -> Rope
    apply (TextDocumentContentChangeEvent (Just rng) _ "") = deleteRange rng
    apply (TextDocumentContentChangeEvent (Just rng) _ t)  = replaceRange rng t
    apply (TextDocumentContentChangeEvent Nothing _ t)     = const $ fromText t
     
didClose :: (MonadState s m, HasDocuments s) => TextDocumentItem -> m ()
didClose (TextDocumentItem u _ _ _) = documents.at u .= Nothing

-- didChangeWatchedFiles :: Monad m => [FileEvent] -> m ()
-- didChangeWatchedFiles events = return ()
