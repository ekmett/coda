{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}

module Coda.Util.Default 
  ( GDefault, gdef
  ) where

import Data.Default
import GHC.Generics

gdef :: (Generic a, GDefault (Rep a)) => a
gdef = to gdef'

class GDefault f where
  gdef' :: f a

instance GDefault U1 where
  gdef' = U1

instance (GDefault f, GDefault g) => GDefault (f :*: g) where
  gdef' = gdef' :*: gdef'

instance Default c => GDefault (K1 i c) where
  gdef' = K1 def

instance GDefault f => GDefault (M1 i c f) where
  gdef' = M1 gdef'
