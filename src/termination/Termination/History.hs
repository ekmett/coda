{-# language GADTs #-}

-- | Accumulated history for a termination check

module Termination.History 
  ( History(..)
  , test
  ) where

import Data.Functor.Contravariant

data History a where
  History :: (s -> a -> Maybe s) -> s -> History a

instance Contravariant History where
  contramap f (History g s) = History (\xs a -> g xs (f a)) s

test :: History a -> a -> Maybe (History a)
test (History k s) a = History k <$> k s a 
