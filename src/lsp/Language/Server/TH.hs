--------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Bibs and bobs that make it easier to derive lenses and aeson instances
--
--------------------------------------------------------------------------------


module Language.Server.TH 
  ( jsonOmit
  , jsonKeep
  , lenses
  ) where

import Control.Lens
import Data.Aeson.TH
import Data.Char
import Data.Foldable
import Data.List (stripPrefix)
import Language.Haskell.TH

jsonOmit, jsonKeep :: Name -> Q [Dec]
jsonOmit = deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 1, omitNothingFields = True }
jsonKeep = deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 1, omitNothingFields = False }

special :: [String]
special = ["data","id","class","type"]

lenses :: Name -> Q [Dec]
lenses = makeLensesWith $ defaultFieldRules & lensField .~ \ _ _ _field -> toList $ do
  field <- stripPrefix "_" (nameBase _field)
  let className = "Has" ++ (field & _head %~ toUpper)
      methodName
        | field `elem` special = field ++ "_"
        | otherwise = field
  return $ MethodName (mkName className) (mkName methodName)
