{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}

module Coda.Syntax.Mode 
  ( Mode(..)
  , HasMode(..)
  ) where 

import GHC.Generics
import Data.Data
import Data.Default
import Data.Semigroup

newtype Mode
  = Mode Int
  deriving (Eq,Ord,Show,Read,Num,Data,Generic)

instance Default Mode where
  def = 0

instance Semigroup Mode where
  x <> 0 = x
  _ <> x = x

instance Monoid Mode where
  mempty = 0
  mappend = (<>)

class HasMode t where
  mode :: t -> Mode

instance HasMode Mode where
  mode = id

