{-# language GADTs #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
module Coda.Automata.Internal where

import Control.Lens
import qualified Coda.Set.Lazy as Set
import Coda.Set.Lazy (Set)
import Data.Functor.Contravariant.Divisible
import Data.Void
import Prelude hiding (product, sum, reverse, concat)

data NFA a where
  NFA :: Ord s => Set s -> Set s -> Set s -> (a -> s -> Set s) -> NFA a

instance Contravariant NFA where
  contramap f (NFA ss is fs d) = NFA ss is fs (d . f)

-- divide computes the intersection of two NFAs
instance Divisible NFA where
  conquer = NFA [()] [()] [()] (\_ _ -> [()])
  divide f (NFA ss is fs d) (NFA ss' is' fs' d')
    = NFA (Set.product ss ss') (Set.product is is') (Set.product fs fs') $ \ a (s,s') -> case f a of
        (b, c) -> Set.product (d b s) (d' c s')

-- decide computes an nfa disjoint union
instance Decidable NFA where
  lose f = NFA ([] :: Set Void) [] [] (absurd . f)
  choose f (NFA ss is fs d) (NFA ss' is' fs' d')
    = NFA (Set.sum ss ss') (Set.sum is is') (Set.sum fs fs') $ \a s -> case f a of
      Left b -> case s of
        Left s' -> Set.mapMonotonic Left (d b s')
        Right{} -> Set.empty
      Right c -> case s of
        Left{} -> Set.empty
        Right s' -> Set.mapMonotonic Right (d' c s')

data DFA a where
  DFA :: Ord s => Set s -> s -> Set s -> (a -> s -> s) -> DFA a

instance Contravariant DFA where
  contramap f (DFA ss is fs d) = DFA ss is fs (d . f)

instance Divisible DFA where
  conquer = DFA [()] () [()] $ \_ _ -> ()
  divide = liftN2 . divide

instance Decidable DFA where
  lose = nfa2dfa . lose
  choose = liftN2 . choose

dfa :: Iso (NFA a) (NFA b) (DFA a) (DFA b)
dfa = iso nfa2dfa dfa2nfa

nfa :: Iso (DFA a) (DFA b) (NFA a) (NFA b)
nfa = iso dfa2nfa nfa2dfa

liftN2 :: (NFA a -> NFA b -> NFA c) -> DFA a -> DFA b -> DFA c
liftN2 f = over nfa . f . dfa2nfa

nfa2dfa :: NFA a -> DFA a
nfa2dfa (NFA ss is fs d) = DFA ss' is (Set.filter (intersects fs) ss') d' where
  ss' = Set.powerset ss
  d' = nondet d

dfa2nfa :: DFA a -> NFA a
dfa2nfa (DFA ss is fs d) = NFA ss (Set.singleton is) fs $ \a s -> Set.singleton $ d a s

-- perform an NFA step
nondet :: Ord s => (a -> s -> Set s) -> a -> Set s -> Set s
nondet f a = foldMap (f a)

-- perform several NFA steps
nondets :: Ord s => (a -> s -> Set s) -> [a] -> Set s -> Set s
nondets f as z = foldr (nondet f) z as

intersects :: Ord s => Set s -> Set s -> Bool
intersects xs ys = not $ Set.null $ Set.intersection xs ys
