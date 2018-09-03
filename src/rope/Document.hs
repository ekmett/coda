{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Document
  ( Document(..)
  , Documents
  , HasDocuments(..)
  , HasLanguageId(..)
  , HasVersion(..)
  , HasContents(..)
  , HasOpen(..)
  , didOpen
  , didChange
  , didSave
  , didClose
  ) where

import Control.Lens
import Control.Monad.State

import Data.Foldable (for_)
import Data.Function (on)
import Data.HashMap.Strict hiding (foldr)
import Data.List (sortBy)
import Data.Text as Text hiding (foldr)

import Language.Server.Protocol
import FingerTree (Measured(..))

import Rope

data Document = Document
  { _languageId :: !Text
  , _version    :: {-# unpack #-} !Int
  , _contents   :: !Rope
  , _open       :: !Bool
  , _changed    :: !Bool -- differs from the contents on disk
  } deriving Show

type Documents = HashMap DocumentUri Document

instance Measured Document where
  type Measure Document = LineMeasure
  measure = views contents measure

makeFieldsNoPrefix ''Document

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
    apply (TextDocumentContentChangeEvent (Just rng) _ "") = deleteRange rng
    apply (TextDocumentContentChangeEvent (Just rng) _ t)  = replaceRange rng t
    apply (TextDocumentContentChangeEvent Nothing _ t)     = const $ fromText t

didSave :: (MonadState s m, HasDocuments s Documents) => DidSaveTextDocumentParams -> m ()
didSave (DidSaveTextDocumentParams t mt) =
  modifying (documents.ix (t^.uri)) $ execState $ do
    for_ mt $ \i -> contents .= fromText i
    changed .= False

didClose :: (MonadState s m, HasDocuments s (HashMap DocumentUri a)) => TextDocumentIdentifier -> m ()
didClose t = documents.at (t^.uri) .= Nothing
