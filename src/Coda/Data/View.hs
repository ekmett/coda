{-# language DeriveTraversable #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Data.View
  ( ViewL(..), ViewR(..)
  , ViewableL(..), ViewableR(..)
  ) where

import Data.Data
import GHC.Generics

data ViewL f a
  = EmptyL
  | !a :< !(f a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Data, Generic, Generic1)

data ViewR f a
  = EmptyR
  | !(f a) :> !a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Data, Generic, Generic1)
    
class ViewableL f where
  viewL :: f a -> ViewL f a

class ViewableR f where
  viewR :: f a -> ViewR f a
