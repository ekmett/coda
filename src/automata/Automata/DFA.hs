{-# language GADTs #-}
{-# language OverloadedLists #-}
module Automata.DFA
  ( DFA(..)
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
import Prelude hiding (reverse, concat)

import Automata.Internal
import qualified Automata.NFA as NFA
import qualified Set.Lazy as Set

reverse :: DFA a -> DFA a
reverse = over nfa NFA.reverse

complement :: DFA a -> DFA a
complement (DFA ss is fs d) = DFA ss is (Set.difference ss fs) d

reversed :: Iso (DFA a) (DFA b) (DFA a) (DFA b)
reversed = iso complement complement

complemented :: Iso (DFA a) (DFA b) (DFA a) (DFA b)
complemented = iso complement complement

star :: DFA a -> DFA a
star = over nfa NFA.star

concat :: DFA a -> DFA a -> DFA a
concat = liftN2 NFA.concat

union :: DFA a -> DFA a -> DFA a
union = liftN2 NFA.union

intersection :: DFA a -> DFA a -> DFA a
intersection = liftN2 NFA.intersection

-- reduce the number of states using knowledge about all possible eventual inputs
shrink :: (Foldable f, Eq a) => f a -> DFA a -> DFA a
shrink as (DFA _ i fs d) = DFA ss' i (Set.intersection fs ss') d where
  ss' = reachable (\a s -> Set.singleton (d a s)) as (Set.singleton i)

size :: DFA a -> Int
size (DFA ss _ _ _) = Set.size ss

--------------------------------------------------------------------------------
-- derivative parsing
--------------------------------------------------------------------------------

-- feed a single prefix
prefix :: a -> DFA a -> DFA a
prefix a (DFA ss i fs d) = DFA ss (d a i) fs d

-- feed a long prefix
prefixes :: [a] -> DFA a -> DFA a
prefixes as (DFA ss i fs d) = DFA ss (foldl (flip d) i as) fs d

-- feed a single suffix
suffix :: DFA a -> a -> DFA a
suffix m a = over nfa (`NFA.suffix` a) m

-- feed a long suffix
suffixes :: DFA a -> [a] -> DFA a
suffixes m as = over nfa (`NFA.suffixes` as) m

-- check to see if we accept the empty string
check :: DFA a -> Bool
check (DFA _ i fs _) = Set.member i fs
