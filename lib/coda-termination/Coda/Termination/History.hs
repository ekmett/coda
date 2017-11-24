{-# language GADTs #-}
{-# language ParallelListComp #-}

-- | Accumulated history for a termination check

module Coda.Termination.History 
  ( History(..)
  , history
  , test
  ) where

import Coda.Termination.Pair
import Coda.Termination.Test
import Data.Functor.Contravariant

data History a where
  History :: (s -> a -> Maybe s) -> s -> History a

instance Contravariant History where
  contramap f (History g s) = History (\xs a -> g xs (f a)) s

history :: Test a -> History a
history (Test p f) = History step [] where
  step xs x
    | any bb1 ys = Nothing
    | otherwise = Just $ z : [ x' | x' <- xs | False <- bb2 <$> ys ] where 
      z  = p x
      ys = f z <$> xs

test :: History a -> a -> Maybe (History a)
test (History k s) a = History k <$> k s a 
