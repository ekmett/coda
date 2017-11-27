{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language BangPatterns #-}
{-# language Strict #-}
module Ersatz.Solver.BDD where

import Control.Applicative
import Data.Bits
import Data.BDD as BDD
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Proxy
import Ersatz.Problem
import Ersatz.Solution

literal :: Cached s => Int -> BDD s
literal 1    = One
literal (-1) = Zero
literal i    = polarize i $ var (abs i)

-- use a more deliberate bdd construction for the clauses that constructs from the bottom up
clause :: Cached s => IntSet -> BDD s
-- clause = IntSet.foldr (\a r -> literal a `BDD.or` r) Zero
clause is = case splitMember 1 is of
  (l,t,r)
     | t -> One -- True is a member of the clause
     | otherwise -> start Zero (IntSet.toAscList $ fst $ IntSet.split (-1) l) (IntSet.toDescList r)
  where
    start acc [] ys = posi acc ys
    start acc xs [] = negi acc xs
    start acc (x:xs) (y:ys) = go acc (negate x) xs y ys
    posi !acc [] = acc
    posi acc (y:ys) = posi (BDD y acc One) ys
    negi !acc [] = acc
    negi acc (x:xs) = negi (BDD (negate x) One acc) xs
    go !acc !x xs !y ys = case compare x y of
      GT | acc' <- BDD x One acc -> case xs of
        []     -> posi (BDD y acc' One) ys
        x':xs' -> go acc' (negate x') xs' y ys
      EQ -> One -- x \/ ~x => this clause is always true
      LT | acc' <- BDD y acc One -> case ys of
        []     -> negi (BDD x One acc') xs
        y':ys' -> go acc' x xs y' ys'

robdd :: Monad m => Solver SAT m
robdd problem = pure $ reifyCache $ \(Proxy :: Proxy s) ->
  let solve = Prelude.foldr (\a r -> clause a .&. r) One

      result :: BDD s
      result = solve (dimacsClauses problem)

      present One  = Just IntMap.empty
      present Zero = Nothing
      present (BDD v l r) = IntMap.insert v False <$> present l
                        <|> IntMap.insert v True  <$> present r
  in case present result of
    Nothing -> (Unsatisfied, IntMap.empty)
    Just s -> (Satisfied, s)
