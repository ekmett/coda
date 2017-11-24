{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Coda.Termination.Class
  ( Termination(..)
  , Termination1(..)
  ) where

import Coda.Termination.Test
import Data.Functor.Contravariant.Generic
import Data.Proxy

class Termination a where
  wqo :: Test a 
  default wqo :: Deciding Termination a => Test a
  wqo = deciding (Proxy :: Proxy Termination) wqo 

instance Termination ()
instance Termination a => Termination [a]
instance Termination a => Termination (Maybe a)
instance (Termination a, Termination b) => Termination (Either a b)
instance (Termination a, Termination b) => Termination (a, b)

class Termination1 f where
  wqo1 :: Test a -> Test (f a)
  default wqo1 :: Deciding1 Termination f => Test a -> Test (f a)
  wqo1 = deciding1 (Proxy :: Proxy Termination) wqo

instance Termination1 [] 
instance Termination1 Maybe
instance Termination e => Termination1 ((,) e)
instance Termination e => Termination1 (Either e)
