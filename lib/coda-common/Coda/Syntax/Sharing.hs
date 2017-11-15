{-# language DeriveTraversable, DeriveDataTypeable, DeriveGeneric #-}
module Coda.Syntax.Sharing
  ( Sharing(..), sharing
  ) where

import Data.Data
import GHC.Generics

data Sharing a = Sharing {-# unpack #-} !Bool a
  deriving (Foldable, Traversable, Generic, Generic1, Data, Eq, Ord, Show, Read)

instance Functor Sharing where
  fmap f (Sharing m a) = Sharing m (f a)
  a <$ Sharing m _ = Sharing m a

instance Applicative Sharing where
  pure = Sharing False
  Sharing m f <*> Sharing n a = Sharing (m || n) (f a)
  Sharing m x <* Sharing n _ = Sharing (m || n) x
  Sharing m _ *> Sharing n x = Sharing (m || n) x

instance Monad Sharing where
  Sharing False a >>= f = f a
  Sharing True a >>= f = case f a of
    Sharing _ b -> Sharing True b
  Sharing m _ >> Sharing n a = Sharing (m || n) a

sharing :: a -> Sharing a -> a
sharing z (Sharing x y) = if x then y else z
