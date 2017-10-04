{-# language TypeFamilies, DeriveTraversable, MultiParamTypeClasses, LambdaCase, FlexibleInstances, TemplateHaskell #-}

module Coda.Data.Bwd 
  ( Bwd(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Zip
import Data.Deriving
import Data.Semigroup
import Data.Traversable
import GHC.Exts

-- snoc-list
data Bwd a = B0 | Bwd a :- a
  deriving (Functor, Traversable, Eq, Ord, Show, Read)

instance Foldable Bwd where
  null B0 = True
  null _ = False

  foldl _ z B0 = z
  foldl f z (as :- a) = foldl f z as `f` a

  foldMap = foldMapDefault

instance IsList (Bwd a) where
  type Item (Bwd a) = a

  fromList = go B0 where
    go acc [] = acc
    go acc (x:xs) = go (acc :- x) xs
  
  toList = go [] where
    go acc B0 = acc
    go acc (xs :- x) = go (x:acc) xs

instance Applicative Bwd where
  pure a = B0 :- a
  (<*>) = ap

instance Monad Bwd where
  (>>=) = flip foldMap

instance Semigroup (Bwd a) where
  (<>) = (<|>)

instance Monoid (Bwd a) where
  mempty = empty
  mappend = (<|>)

instance Alternative Bwd where
  empty = B0
  as <|> B0 = as
  as <|> (bs :- b) = (as <|> bs) :- b

instance MonadPlus Bwd where
  mzero = empty
  mplus = (<|>)

instance Snoc (Bwd a) (Bwd b) a b where
  _Snoc = prism (uncurry (:-)) $ \case
    B0 -> Left B0
    xs :- x -> Right (xs, x)

instance AsEmpty (Bwd a) where
  _Empty = nearly B0 null

instance MonadZip Bwd where
  mzipWith _ B0 _ = B0
  mzipWith _ _ B0 = B0
  mzipWith f (as :- a) (bs :- b) = mzipWith f as bs :- f a b

deriveEq1 ''Bwd
deriveOrd1 ''Bwd
deriveShow1 ''Bwd
deriveRead1 ''Bwd
