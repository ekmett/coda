{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Stuff we an measure in UTF-16 code units
---------------------------------------------------------------------------------

module Coda.Relative.Delta.Type
  ( Delta(..)
  ) where

import Data.Data
import Data.Default
import Data.Hashable
import Data.Semigroup
import GHC.Generics
import Text.Read
import Prelude

-- | A count of UTF-16 code-units.
--
-- This forms an (obvious) Abelian group unlike
-- the merely monoidal pairs of line and column.
--
-- It is also very compact fitting in a single 'Int'.
newtype Delta = Delta Int
  deriving (Eq, Ord, Data, Generic, Num)

instance Show Delta where
  showsPrec d (Delta n) = showsPrec d n

instance Read Delta where
  readPrec = Delta <$> readPrec

instance Hashable Delta

instance Default Delta where
  def = Delta def

instance Semigroup Delta where
  (<>) = (+)

instance Monoid Delta where
  mempty = 0
  mappend = (+)
