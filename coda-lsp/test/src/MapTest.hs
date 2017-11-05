{-# language StandaloneDeriving #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
{-# options_ghc -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD2 (see the file LICENSE.md)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module MapTest where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Map as Relative
import Data.Map.Strict as Model
import GHC.Generics
import Test.QuickCheck

deriving instance Arbitrary Delta

type Key = Delta
type Value = Delta

data Model
  = Union Model Model
  | Insert Key Value Model
  | Rel Delta Model
--  | Delete Key Model
  | Empty
  deriving (Eq,Show,Generic)

instance Arbitrary Model where
  arbitrary = oneof
    [ 
  --   Union <$> arbitrary <*> arbitrary
      Insert <$> arbitrary <*> arbitrary <*> arbitrary
    , Rel <$> arbitrary <*> arbitrary
    , pure Empty
    ]
  shrink = genericShrink

model :: Model -> Model.Map Key Value
model (Union xs ys) = model xs `mappend` model ys
model (Insert k v as) = Model.insert k v $ model as
model (Rel d as) = Model.fromList $ frel d $ Model.toList $ model as
model Empty = mempty

eval :: Model -> Relative.Map Key Value
eval (Union xs ys) = eval xs `mappend` eval ys
eval (Insert k v as) = Relative.insert k v $ eval as
eval (Rel d as) = rel d $ eval as
eval Empty = mempty

prop_map :: Model -> Property
prop_map x = Relative.toAscList (eval x) === Model.toAscList (model x)

prop_map_1 :: Property
prop_map_1 = prop_map $ Insert 1 2 (Rel 4 (Insert 8 16 Empty))

prop_map_2 :: Property
prop_map_2 = prop_map $ Insert 1 4 (Rel 1 (Insert 0 1 Empty))

prop_map_3 :: Property
prop_map_3 = prop_map $ Insert 1 0 (Rel 1 (Insert 1 0 (Insert 0 0 Empty)))

prop_map_4 :: Property
prop_map_4 = prop_map $ Union (Insert 0 0 (Insert 1 0 Empty)) (Rel 1 (Insert 1 0 (Insert 0 0 Empty)))
