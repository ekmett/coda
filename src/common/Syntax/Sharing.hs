{-# language DeriveTraversable, DeriveDataTypeable, DeriveGeneric #-}
module Syntax.Sharing
  ( Sharing(..), sharing, changed
  ) where

import Data.Bits
import Data.Data
import GHC.Generics

data Sharing a = Sharing {-# UNPACK #-} !Int a
  deriving (Foldable, Traversable, Generic, Generic1, Data, Eq, Ord, Show, Read)

instance Functor Sharing where
  fmap f (Sharing m a) = Sharing m (f a)
  a <$ Sharing m _ = Sharing m a

instance Applicative Sharing where
  pure = Sharing 0
  Sharing m f <*> Sharing n a = Sharing (m .|. n) (f a)
  Sharing m x <* Sharing n _ = Sharing (m .|. n) x
  Sharing m _ *> Sharing n x = Sharing (m .|. n) x

instance Monad Sharing where
  Sharing 0 a >>= f = f a
  Sharing _ a >>= f = case f a of
    Sharing _ b -> Sharing 1 b
  Sharing m _ >> Sharing n a = Sharing (m .|. n) a

changed :: a -> Sharing a
changed = Sharing 1

sharing :: a -> Sharing a -> a
sharing z (Sharing 0 _) = z
sharing _ (Sharing _ y) = y
