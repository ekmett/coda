-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD2 (see the file LICENSE.md)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Coda.Message.Sink 
  ( Sink(..)
  ) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

-- | A contravariant message sink
newtype Sink a = Sink { (<~) :: a -> IO () }

instance Contravariant Sink where
  contramap f (Sink g) = Sink (g . f)

instance Divisible Sink where
  divide f (Sink g) (Sink h) = Sink $ \a -> case f a of
    (l, r) -> g l *> h r
  conquer = Sink $ const $ pure ()

instance Decidable Sink where
  lose f = Sink (absurd . f)   
  choose f (Sink g) (Sink h) = Sink (either g h . f)
