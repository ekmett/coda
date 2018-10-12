{-# language GADTs #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
module Automata.NFA
  ( NFA(..)
  , reverse, reversed
  , complement, complemented
  , union
  , intersection
  , concat
  , star
  , shrink
  , size
  -- derivative parsing
  , prefix, prefixes
  , suffix, suffixes
  , check
  ) where

import Control.Lens hiding (reversed)
import qualified Data.List as List
import Prelude hiding (reverse, concat)

import Automata.Internal
import qualified Set.Lazy as Set

-- nfa reversal
reverse :: NFA a -> NFA a
reverse (NFA ss i f d) = NFA ss f i $ \ a t -> Set.filter (Set.member t . d a) ss

-- nfa complement
complement :: NFA a -> NFA a
complement = dfa2nfa . go . nfa2dfa where
  go (DFA ss is fs d) = DFA ss is (Set.difference ss fs) d

reversed :: Iso (NFA a) (NFA b) (NFA a) (NFA b)
reversed = iso complement complement

complemented :: Iso (NFA a) (NFA b) (NFA a) (NFA b)
complemented = iso complement complement

-- kleene star
star :: NFA a -> NFA a
star (NFA ss is fs d) = NFA ss is fs $ \a (d a -> r) ->
  if intersects fs r
  then Set.union r is
  else r

-- concatenate two automata
concat :: NFA a -> NFA a -> NFA a
concat (NFA ss is fs d)
       (NFA ss' (Set.mapMonotonic Right -> is') (Set.mapMonotonic Right -> fs') d')
  = NFA (Set.sum ss ss') (Set.mapMonotonic Left is) fs' $ \a s -> case s of
    Right s' -> Set.mapMonotonic Right (d' a s')
    Left (d a -> r) | r' <- Set.mapMonotonic Left r ->
      if intersects r fs
      then Set.union r' is'
      else r'

-- nfa union
union :: NFA a -> NFA a -> NFA a
union (NFA ss is fs d) (NFA ss' is' fs' d')
  = NFA (Set.sum ss ss') (Set.sum is is') (Set.sum fs fs') $ \a s -> case s of
    Left s' -> Set.mapMonotonic Left (d a s')
    Right s' -> Set.mapMonotonic Right (d' a s')

-- nfa intersection
intersection :: NFA a -> NFA a -> NFA a
intersection (NFA ss is fs d) (NFA ss' is' fs' d')
  = NFA (Set.product ss ss') (Set.product is is') (Set.product fs fs') $ \ a (s,s') -> Set.product (d a s) (d' a s')

-- reduce the number of states using knowledge about all possible eventual inputs
shrink :: (Foldable f, Eq a) => f a -> NFA a -> NFA a
shrink as (NFA _ is fs d) = NFA ss' is (Set.intersection fs ss') d where
  ss' = reachable d as is
  -- TODO: filter d w/ as to avoid bogus states?

size :: NFA a -> Int
size (NFA ss _ _ _) = Set.size ss

--------------------------------------------------------------------------------
-- derivative parsing
--------------------------------------------------------------------------------

-- feed a single prefix
prefix :: a -> NFA a -> NFA a
prefix a (NFA ss is fs d) = NFA ss (nondet d a is) fs d

-- feed a long prefix
prefixes :: [a] -> NFA a -> NFA a
prefixes as (NFA ss is fs d) = NFA ss (nondets d as is) fs d

-- feed a single suffix
suffix :: NFA a -> a -> NFA a
suffix (NFA ss is fs d) a = NFA ss is (Set.filter (intersects fs . d a) ss) d

-- feed a long suffix
suffixes :: NFA a -> [a] -> NFA a
suffixes (NFA ss is fs d) as = NFA ss is (nondets d' (List.reverse as) fs) d where
  d' a t = Set.filter (Set.member t . d a) ss

-- check to see if we accept the empty string
check :: NFA a -> Bool
check (NFA _ is fs _) = intersects is fs
