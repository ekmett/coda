{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
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
module Coda.Message.Severity
  ( Severity(..)
  , pattern Error
  , pattern Warning
  , pattern Information
  , pattern Hint
  ) where

import Control.Applicative
import Data.Aeson hiding (Error)
import Data.Data
import Data.Hashable
import Data.Ix
import GHC.Generics (Generic)
import Text.Read

-- |
-- See @DiagnosticSeverity@ in 
--
-- <https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#diagnostic>
newtype Severity = Severity Int
  deriving (Eq,Ord,Enum,Bounded,Ix,Data,Generic)

instance ToJSON Severity
instance FromJSON Severity
instance Hashable Severity

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
  readPrec = parens 
      $ do { Ident "Error"       <- lexP; return Error       }
    <|> do { Ident "Warning"     <- lexP; return Warning     }
    <|> do { Ident "Information" <- lexP; return Information }
    <|> do { Ident "Hint"        <- lexP; return Hint        }
    <|> do prec 10 $ do { Ident "Severity" <- lexP; Severity <$> step readPrec }
