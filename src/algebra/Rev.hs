{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Rev 
  ( Rev(..)
  , _Rev
  ) where

import Relative.Class
import Control.Lens
import Data.Default
import Data.Semigroup

-- reversing a catenable list, etc.
newtype Rev f a
  = Rev { runRev :: f a }
  deriving (Eq,Ord,Show,Read)

makePrisms ''Rev
makeWrapped ''Rev

instance AsEmpty (f a) => AsEmpty (Rev f a) where
  _Empty = _Wrapped._Empty

instance Snoc (f a) (f b) a b => Cons (Rev f a) (Rev f b) a b where
  _Cons = _Wrapped._Snoc.swapped.mapping (from _Wrapped)

instance Cons (f a) (f b) a b => Snoc (Rev f a) (Rev f b) a b where
  _Snoc = _Wrapped._Cons.mapping (from _Wrapped).swapped

instance Default (f a) => Default (Rev f a) where
  def = Rev def

instance Semigroup (f a) => Semigroup (Rev f a) where
  Rev a <> Rev b = Rev (b <> a)

instance Monoid (f a) => Monoid (Rev f a) where
  mempty = Rev mempty
  mappend (Rev a) (Rev b) = Rev (mappend b a)

instance Relative (f a) => Relative (Rev f a) where rel d (Rev m) = Rev (rel d m)

instance RelativeMonoid (f a) => RelativeMonoid (Rev f a)
