{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

module Coda.Syntax.Document
  ( Document(..)
  , Documents, HasDocuments(..)
  , HasLanguageId(..)
  , HasVersion(..)
  , HasContents(..)
  , HasOpen(..)
  , didOpen
  , didChange
  , didSave
  , didClose
  ) where

import Coda.Syntax.Rope
import Control.Lens
import Control.Monad.State
import Data.Foldable (for_)
import Data.Function (on)
import Data.HashMap.Strict hiding (foldr)
import Data.List (sortBy)
import Data.Text as Text hiding (lines, foldr)
import Language.Server.Protocol
import Prelude hiding (lines)

data Document = Document
  { _languageId :: !Text
  , _version    :: {-# unpack #-} !Int
  , _contents   :: !Rope
  , _open       :: !Bool
  , _changed    :: !Bool -- differs than the contents on disk
  } deriving Show

makeFieldsNoPrefix ''Document

type Documents = HashMap DocumentUri Document

class HasDocuments t d | t -> d where
  documents :: Lens' t d

didOpen :: (MonadState s m, HasDocuments s Documents) => TextDocumentItem -> m ()
didOpen (TextDocumentItem u l v t) = documents.at u ?= Document l v (fromText t) True False

didChange :: (MonadState s m, HasDocuments s Documents) => DidChangeTextDocumentParams -> m ()
didChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier u v) cs) =
  modifying (documents.ix u) $ execState $ do
    changed .= True
    version .= v
    modifying contents $ foldr apply ?? sortBy (compare `on` view range) cs
  where
    apply :: TextDocumentContentChangeEvent -> Rope -> Rope
    apply (TextDocumentContentChangeEvent (Just rng) _ "") = deleteRange rng
    apply (TextDocumentContentChangeEvent (Just rng) _ t)  = replaceRange rng t
    apply (TextDocumentContentChangeEvent Nothing _ t)     = const $ fromText t

didSave :: (MonadState s m, HasDocuments s Documents) => DidSaveTextDocumentParams -> m ()
didSave (DidSaveTextDocumentParams t mt) =
  modifying (documents.ix (t^.uri)) $ execState $ do
    for_ mt $ \i -> contents .= fromText i
    changed .= False

didClose :: (MonadState s m, HasDocuments s Documents) => TextDocumentIdentifier -> m ()
didClose t = documents.at (t^.uri) .= Nothing
