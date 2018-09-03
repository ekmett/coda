{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language OverloadedStrings #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Syntax.Name
  ( Name(Qualified, Unqualified, QVarId, QConId, QVarOp, QConOp, VarId, ConId, VarOp, ConOp)
  , HasOperator(..)
  , HasConstructor(..)
  , HasIdent(..)
  , HasQualifier(..)
  ) where

import Control.Lens
import Data.Data
import Data.Text
import GHC.Generics hiding (prec)
import Text.Read

data Name
  = Qualified   { _operator :: !Bool, _constructor :: !Bool, _qualifier :: !Text, _ident :: !Text }
  | Unqualified { _operator :: !Bool, _constructor :: !Bool, _ident :: !Text }
  deriving (Eq,Ord,Data,Generic)
{-# complete QConId, QVarId, QConOp, QVarOp, ConId, VarId, ConOp, VarOp #-}

pattern QVarId :: Text -> Text -> Name
pattern QVarId q i = Qualified True False q i

pattern QConId :: Text -> Text -> Name
pattern QConId q i = Qualified True True q i

pattern QVarOp :: Text -> Text -> Name
pattern QVarOp q i = Qualified False False q i

pattern QConOp :: Text -> Text -> Name
pattern QConOp q i = Qualified False True q i

pattern VarId :: Text -> Name
pattern VarId i = Unqualified True False i

pattern ConId :: Text -> Name
pattern ConId i = Unqualified True True i

pattern VarOp :: Text -> Name
pattern VarOp i = Unqualified False False i

pattern ConOp :: Text -> Name
pattern ConOp i = Unqualified False True i

instance Show Name where
  showsPrec d (QVarId q n) = showParen (d > 10) $ showString "QVarId " . showsPrec 11 q . showChar ' ' . showsPrec 11 n
  showsPrec d (QConId q n) = showParen (d > 10) $ showString "QConId " . showsPrec 11 q . showChar ' ' . showsPrec 11 n
  showsPrec d (QVarOp q n) = showParen (d > 10) $ showString "QVarOp " . showsPrec 11 q . showChar ' ' . showsPrec 11 n
  showsPrec d (QConOp q n) = showParen (d > 10) $ showString "QConOp " . showsPrec 11 q . showChar ' ' . showsPrec 11 n
  showsPrec d (VarId n)    = showParen (d > 10) $ showString "VarId " . showsPrec 11 n
  showsPrec d (ConId n)    = showParen (d > 10) $ showString "ConId " . showsPrec 11 n
  showsPrec d (VarOp n)    = showParen (d > 10) $ showString "VarOp " . showsPrec 11 n
  showsPrec d (ConOp n)    = showParen (d > 10) $ showString "ConOp " . showsPrec 11 n

instance Read Name where
  readPrec = parens 
      $ prec 10 (do Ident "QVarId" <- lexP; QVarId <$> step readPrec <*> step readPrec)
    +++ prec 10 (do Ident "QConId" <- lexP; QConId <$> step readPrec <*> step readPrec)
    +++ prec 10 (do Ident "QVarOp" <- lexP; QVarOp <$> step readPrec <*> step readPrec)
    +++ prec 10 (do Ident "QConOp" <- lexP; QConOp <$> step readPrec <*> step readPrec)
    +++ prec 10 (do Ident "VarId" <- lexP; VarId <$> step readPrec)
    +++ prec 10 (do Ident "ConId" <- lexP; ConId <$> step readPrec)
    +++ prec 10 (do Ident "VarOp" <- lexP; VarOp <$> step readPrec)
    +++ prec 10 (do Ident "ConOp" <- lexP; ConOp <$> step readPrec)

makeFieldsNoPrefix ''Name
