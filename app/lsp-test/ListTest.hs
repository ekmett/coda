{-# language StandaloneDeriving #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedLists #-}
{-# language DeriveGeneric #-}
{-# options_ghc -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2017-2018
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module ListTest where

import Control.Lens as Lens
import Data.List as Model
import GHC.Generics
import Test.QuickCheck

import Relative.Class
import Relative.Delta
import Relative.List as List

deriving instance Arbitrary Delta

data ListModel
  = Concat ListModel ListModel
  | Push Delta ListModel
  | Rel Delta ListModel
  | Reverse ListModel
  | Drop1 ListModel
  | NilModel
  deriving (Eq,Show,Generic)

instance Arbitrary ListModel where
  arbitrary = oneof
    [ Concat <$> arbitrary <*> arbitrary
    , Push <$> arbitrary <*> arbitrary
    , Rel <$> arbitrary <*> arbitrary
    , Reverse <$> arbitrary
    , Drop1 <$> arbitrary
    , pure NilModel
    ]
  shrink = genericShrink

model :: ListModel -> [Delta]
model (Concat xs ys) = model xs `mappend` model ys
model (Push a as) = a : model as
model (Rel d as) = fmap (rel d) (model as)
model (Reverse as) = Model.reverse (model as)
model NilModel = []
model (Drop1 as) = Model.drop 1 (model as)

eval :: ListModel -> List Delta
eval (Concat xs ys) = eval xs `mappend` eval ys
eval (Push a as) = cons a (eval as)
eval (Rel d as) = rel d (eval as)
eval (Reverse as) = List.reverse (eval as)
eval NilModel = []
eval (Drop1 as) = case Lens.uncons (eval as) of
  Just (_, as') -> as'
  Nothing -> []

prop_list :: ListModel -> Property
prop_list x = unfoldr Lens.uncons (eval x) === model x
