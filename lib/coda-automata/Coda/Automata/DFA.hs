{-# language GADTs #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
module Coda.Automata.DFA
  ( DFA(..)
  , reverse
  , complement
  , union
  , intersection
  , concat
  , star
  -- derivative parsing
  , prefix, prefixes
  , suffix, suffixes
  , accepts
  ) where

import Coda.Automata.Internal
import qualified Coda.Automata.NFA as NFA
import qualified Coda.Set.Lazy as Set
import Control.Lens
import Prelude hiding (product, sum, reverse, concat)

reverse :: DFA a -> DFA a
reverse = over nfa NFA.reverse

complement :: DFA a -> DFA a
complement (DFA ss is fs d) = DFA ss is (Set.difference ss fs) d

star :: DFA a -> DFA a
star = over nfa NFA.star

concat :: DFA a -> DFA a -> DFA a
concat m = over nfa (NFA.concat $ dfa2nfa m)

union :: DFA a -> DFA a -> DFA a
union m = over nfa (NFA.union $ dfa2nfa m)

intersection :: DFA a -> DFA a -> DFA a
intersection m = over nfa (NFA.intersection $ dfa2nfa m)

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
accepts :: DFA a -> Bool
accepts (DFA _ i fs _) = Set.member i fs
