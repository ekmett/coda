{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

module Coda.Syntax.Document
  ( Document(..)
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

import Coda.Syntax.Line
import Coda.Syntax.Rope
import Control.Lens
import Control.Monad.State
import Data.FingerTree (Measured(..))
import Data.Foldable (for_)
import Data.Function (on)
import Data.HashMap.Strict hiding (foldr)
import Data.List (sortBy)
import Data.Text as Text hiding (lines, foldr)
import Language.Server.Protocol
import Prelude hiding (lines)

data Document v a = Document
  { _languageId :: !Text
  , _version    :: {-# unpack #-} !Int
  , _contents   :: !(Rope v a)
  , _open       :: !Bool
  , _changed    :: !Bool -- differs than the contents on disk
  } deriving Show

instance Measured v a => Measured (LineMeasure v) (Document v a) where
  measure = views contents measure

makeFieldsNoPrefix ''Document

class HasDocuments t d | t -> d where
  documents :: Lens' t d

didOpen ::
  ( MonadState s m
  , HasDocuments s (HashMap DocumentUri (Document v a))
  , Measured v a
  , FromText a
  ) => TextDocumentItem -> m ()
didOpen (TextDocumentItem u l v t) = documents.at u ?= Document l v (fromText t) True False

didChange ::
  ( MonadState s m
  , HasDocuments s (HashMap DocumentUri (Document v a))
  , Measured v a
  , FromText a
  ) => DidChangeTextDocumentParams -> m ()
didChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier u v) cs) =
  modifying (documents.ix u) $ execState $ do
    changed .= True
    version .= v
    modifying contents $ foldr apply ?? sortBy (compare `on` view range) cs
  where
    apply (TextDocumentContentChangeEvent (Just rng) _ "") = deleteRange rng
    apply (TextDocumentContentChangeEvent (Just rng) _ t)  = replaceRange rng t
    apply (TextDocumentContentChangeEvent Nothing _ t)     = const $ fromText t

didSave ::
  ( MonadState s m
  , HasDocuments s (HashMap DocumentUri (Document v a))
  , Measured v a
  , FromText a
  ) => DidSaveTextDocumentParams -> m ()
didSave (DidSaveTextDocumentParams t mt) =
  modifying (documents.ix (t^.uri)) $ execState $ do
    for_ mt $ \i -> contents .= fromText i
    changed .= False

didClose ::
  ( MonadState s m
  , HasDocuments s (HashMap DocumentUri a)
  ) => TextDocumentIdentifier -> m ()
didClose t = documents.at (t^.uri) .= Nothing
