module Syntax.Name
  ( Name(..)
  , isBound, isFree
  ) where

import Data.String
-- import Data.Text.Prettyprint.Doc
import Data.Text.Short as T
-- import Console.Pretty

data Name
  = Free ShortText Integer
  | Bound Integer Integer
  deriving (Eq,Ord,Show)

isBound :: Name -> Bool
isBound Bound{} = True
isBound _ = False

isFree :: Name -> Bool
isFree Free{} = True
isFree _ = False

{-
instance Fancy Name where
  fancy (Free x n) | T.null x = annotate StyleName $ pretty '_' <> pretty n
  fancy (Free x 0)  = annotate StyleName $ pretty (toText x)
  fancy (Free x n)  = annotate StyleName $ pretty (toText x) <> pretty n
  fancy (Bound x y) = annotate StyleName $ pretty x <> pretty '@' <> pretty y
-}

instance IsString Name where
  fromString n = Free (T.fromString n) 0
