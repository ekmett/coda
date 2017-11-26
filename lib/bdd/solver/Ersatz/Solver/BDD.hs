{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
module Ersatz.Solver.BDD where

import Control.Applicative
import Data.BDD as BDD
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Proxy
import Ersatz.Problem
import Ersatz.Solution

-- robdd :: Monad m => SAT -> m (Result, IntMap Bool)
robdd :: Monad m => Solver SAT m
robdd problem = pure $ reifyCache $ \(Proxy :: Proxy s) ->
  let literal 1    = One
      literal (-1) = Zero
      literal i    = if i > 0 then var i else neg (var i)
      clause = IntSet.foldr (\a r -> literal a `BDD.or` r) Zero
      solve = Prelude.foldr (\a r -> clause a `BDD.and` r) One
      result :: BDD s
      result = solve (dimacsClauses problem)
      present One  = Just IntMap.empty
      present Zero = Nothing
      present (BDD v l r) = IntMap.insert v False <$> present l
                        <|> IntMap.insert v True  <$> present r
  in case present result of
    Nothing -> (Unsatisfied, IntMap.empty)
    Just s -> (Satisfied, s)
