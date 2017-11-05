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
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

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

import Coda.Relative.Class
import Coda.Syntax.Rope
import Control.Lens
import Control.Monad.State
import Coda.FingerTree (Measured(..))
import Data.Foldable (for_)
import Data.Function (on)
import Data.HashMap.Strict hiding (foldr)
import Data.List (sortBy)
import Data.Text as Text hiding (foldr)
import Language.Server.Protocol

data Document a = Document
  { _languageId :: !Text
  , _version    :: {-# unpack #-} !Int
  , _contents   :: !(Rope a)
  , _open       :: !Bool
  , _changed    :: !Bool -- differs from the contents on disk
  } deriving Show

instance (RelativeMonoid (Measure a), Measured a) => Measured (Document a) where
  type Measure (Document a) = Measure (Rope a)
  measure = views contents measure

makeFieldsNoPrefix ''Document

class HasDocuments t d | t -> d where
  documents :: Lens' t d

didOpen ::
  ( MonadState s m
  , HasDocuments s (HashMap DocumentUri (Document a))
  , RelativeMonoid (Measure a)
  , Measured a
  , FromText a
  ) => TextDocumentItem -> m ()
didOpen (TextDocumentItem u l v t) = documents.at u ?= Document l v (fromText t) True False

didChange ::
  ( MonadState s m
  , HasDocuments s (HashMap DocumentUri (Document a))
  , RelativeMonoid (Measure a)
  , Measured a
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
  , HasDocuments s (HashMap DocumentUri (Document a))
  , RelativeMonoid (Measure a)
  , Measured a
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
