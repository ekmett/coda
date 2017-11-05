{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language RoleAnnotations #-}
{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: non-portable
---
---------------------------------------------------------------------------------

module Coda.Relative.List
  ( List(..)
  , pattern Cons
  , reverse
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Control.Lens (AsEmpty(..), prism, Cons(..))
import Data.Default
import Data.Function (on)
import Data.Semigroup
import GHC.Exts as Exts
import qualified Prelude
import Prelude hiding (reverse)
import Text.Read

-- | A list with an /O(1)/ 'rel', 'cons' and 'uncons', but /O(n)/ ('<>')
data List a = List !Delta [a]

type role List nominal

instance Relative (List a) where
  rel 0 xs = xs
  rel d (List d' as) = List (d <> d') as
  {-# inline rel #-}

pattern Cons :: Relative a => () => a -> List a -> List a
pattern Cons a as <- List d ((rel d -> a):(List d -> as)) where
  Cons a (List d as) = List d (rel (-d) a:as)

reverse :: List a -> List a
reverse (List d as) = List d (Prelude.reverse as)
{-# inline reverse #-}

instance (Show a, Relative a) => Show (List a) where
  showsPrec d = showsPrec d . Exts.toList

instance (Read a, Relative a) => Read (List a) where
  readPrec = Exts.fromList <$> readPrec

instance (Eq a, Relative a) => Eq (List a) where
  (==) = (==) `on` Exts.toList
  {-# inline (==) #-}

instance (Ord a, Relative a) => Ord (List a) where
  compare = compare `on` Exts.toList
  {-# inline compare #-}

instance RelativeOrder a => RelativeOrder (List a)
instance StrictRelativeOrder a => StrictRelativeOrder (List a)
instance Relative a => RelativeMonoid (List a)

instance Default (List a) where
  def = List 0 []

-- /O(n)/
instance Relative a => Semigroup (List a) where
  List d as <> List d' bs | d'' <- d'-d = List d $ as ++ map (rel d'') bs
  {-# inline (<>) #-}

-- /O(n)/
instance Relative a => Monoid (List a) where
  mempty = List 0 []
  mappend = (<>)

instance Relative a => IsList (List a) where
  type Item (List a) = a
  fromList = List 0
  {-# inline conlike fromList #-}
  toList (List d xs) = map (rel d) xs
  {-# inline toList #-}

instance AsEmpty (List a) where
  _Empty = prism (const def) $ \case
    List _ [] -> Right ()
    xs        -> Left xs
  {-# inline _Empty #-}

instance (Relative a, Relative b) => Cons (List a) (List b) a b where
  _Cons = prism (\(a,List d as) -> List d (rel (-d) a:as)) $ \case
    List _ []     -> Left mempty
    List d (a:as) -> Right (rel d a, List d as)
  {-# inline _Cons #-}
