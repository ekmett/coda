{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language FlexibleInstances #-}
{-# options_ghc -Wno-redundant-constraints #-} -- this warning can die in a fire

module Coda.Algebra where

import Data.Semigroup

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

-- | @
-- inv (a <> b) = inv b <> inv a
-- a <> inv a <> a = a
-- inv a = inv a <> a <> inv a
-- inv (inv a) = a
-- @
class Semigroup a => RegularSemigroup a where
  inv :: a -> a

-- | pushout
class (RegularSemigroup a, Monoid a) => RegularMonoid a
instance (RegularSemigroup a, Monoid a) => RegularMonoid a

-- | a regular semigroup where all idempotents commute
--
-- @
-- and [ x <> y == y <> x | x <- [ a <> inv a, inv a <> a ] | y <- [ b <> inv b, inv b <> b ] ]
-- @
class RegularSemigroup a => InverseSemigroup a

-- | pushout
class (InverseSemigroup a, Monoid a) => InverseMonoid a
instance (InverseSemigroup a, Monoid a) => InverseMonoid a

-- | A unipotent inverse monoid
--
-- @a <> inv a = mempty = inv a <> a@
class InverseMonoid a => Group a

--------------------------------------------------------------------------------
-- Ordered algebraic structures
--------------------------------------------------------------------------------

-- |
-- @x '<=' y@ implies @z '<>' x '<=' z '<>' y@ and @x '<>' z '<=' y' <>' z@
class (Ord a, Semigroup a) => OrderedSemigroup a where

-- | pushout
class    (OrderedSemigroup a, Monoid a) => OrderedMonoid a
instance (OrderedSemigroup a, Monoid a) => OrderedMonoid a

-- | pushout
class    (OrderedMonoid a, Group a) => OrderedGroup a
instance (OrderedMonoid a, Group a) => OrderedGroup a

-- |
-- @x '<' y@ implies @z '<>' x '<' z '<>' y@ and @x '<>' z '<' y' <>' z@
-- @x '=' y@ implies @z '<>' x '=' z '<>' y@ and @x '<>' z '=' y' <>' z@
class OrderedSemigroup a => StrictlyOrderedSemigroup a where

-- | pushout
class    (OrderedMonoid a, StrictlyOrderedSemigroup a) => StrictlyOrderedMonoid a
instance (OrderedMonoid a, StrictlyOrderedSemigroup a) => StrictlyOrderedMonoid a

-- | pushout
class    (OrderedGroup a, StrictlyOrderedMonoid a) => StrictlyOrderedGroup a
instance (OrderedGroup a, StrictlyOrderedMonoid a) => StrictlyOrderedGroup a

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

-- | @
-- act (a <> b) = act a . act b
-- @
class Semigroup a => SemigroupAction a b where
  act :: a -> b -> b

-- | @
-- act mempty = id
-- @
class (Monoid a, SemigroupAction a b) => MonoidAction a b

-- pushout
class    (Group a, MonoidAction a b) => GroupAction a b
instance (Group a, MonoidAction a b) => GroupAction a b

--------------------------------------------------------------------------------
-- Monotone Actions
--------------------------------------------------------------------------------

-- | 'act' is strictly monotone in the second argument
--
-- The action of any given element of the semigroup is a monotone function
--
-- @
-- x <= y ==> act a x <= act a y
-- @
class (SemigroupAction a b, Ord b) => MonotoneAction a b

-- | 'act' is strictly monotone in its second argument
--
-- The action of any given element of the semigroup is a strictly monotone function
--
-- @x < y@ implies @act a x < act a y@
-- @x = y@ implies @act a x = act a y@
class MonotoneAction a b => StrictlyMonotoneAction a b

-- | 'act is monotone in first argument
--
-- @a <= b@ implies @act a x <= act b x@
class (SemigroupAction a b, OrderedSemigroup a, Ord b) => OrderedMonotoneAction a b where

-- | 'act is strictly monotone in first argument
--
-- @a = b@ implies @act a x = act b x@
-- @a < b@ implies @act a x < act b x@
class OrderedMonotoneAction a b => OrderedStrictlyMonotoneAction a b

--------------------------------------------------------------------------------
-- Distributive and Unitary Actions
--------------------------------------------------------------------------------

-- | @
-- act a (b <> c) = act a b <> act a c
-- @
class (SemigroupAction a b, Semigroup b) => DistributiveAction a b

-- | @
-- act a mempty = mempty
-- @
--
-- Lemma "inverse action": If a and b are groups, then @inv (act a b) = act a (inv b)@
-- Proof:
--
-- @
-- act a (inv b) <> act a b
-- = act a (inv b <> b) -- distributive action
-- = act a mempty -- left identity
-- = mempty -- unitary action
-- @
--
-- @
-- act a b <> act a (inv b)
-- = act a (b <> inv b) -- distributive action
-- = act a mempty -- right identity
-- = mempty -- unitary action
-- @
--
-- Hence, @act a (inv b)@ is an inverse of @act a b@, and inverses are unique.
--
-- Therefore, @inv (act a b) = act a (inv b)@.
class (DistributiveAction a b, Monoid b) => UnitaryAction a b

-- | @
-- act a (act b c) = act (act a b) (act a c)
-- @
--
-- Hint: Compare the law for how these distributive actions relate to the law for a single distributive action
class (DistributiveAction a b, DistributiveAction a c, DistributiveAction b c) => DistributiveActionAction a b c

--------------------------------------------------------------------------------
-- Semi-direct products of semigroups, monoids and/or groups
--------------------------------------------------------------------------------

-- | strict semidirect product
data Semi a b = Semi !a !b deriving (Eq, Ord, Show, Read)

-- |
-- @
-- Semi a b <> (Semi c d <> Semi e f)
-- = Semi a b <> Semi (c <> e) (d <> act c f) -- definition
-- = Semi (a <> (c <> e)) (b <> act a (d <> act c f)) -- definition
-- = Semi ((a <> c) <> e) (b <> act a (d <> act c f)) -- associativity
-- = Semi ((a <> c) <> e) (b <> (act a d <> act a (act c f))) -- distributive action
-- = Semi ((a <> c) <> e) ((b <> act a d) <> act a (act c f)) -- associativity
-- = Semi ((a <> c) <> e) ((b <> act a d) <> act (a <> c) f) -- semigroup action
-- = Semi (a <> c) (b <> act a d) <> Semi e f -- definition (backwards)
-- = (Semi a b <> Semi c d) <> Semi e f -- definition (backards)
-- @
instance DistributiveAction a b => Semigroup (Semi a b) where
  Semi a x <> Semi b y = Semi (a <> b) (x <> act a y)

-- Requires: @x '<=' y@ implies @z '<>' x '<=' z '<>' y@ and @x '<>' z '<=' y' <>' z@
--
-- Assume @Semi xa xb <= Semi ya yb@.
--
-- Lexicographical ordering implies xa <= ya and if xa = ya, xb <= yb. Otherwise xa < ya.
--
-- By cases: First assume xa = ya, xb <= yb:
--
-- Given this assumption, by monotone action @xb <= yb@ implies @act za xb <= act za yb@
-- Then by ordered semigroup @act za xb <= act za yb@ implies @zb <> act za xb <= zb <> act za yb@
--
-- @
-- Semi za zb <> Semi xa xb
-- = Semi (za <> xa) (zb <> act za xb) -- definition
-- = Semi (za <> ya) (zb <> act za xb) -- xa = ya assumption, and @a@ is a strictly ordered semigroup
-- <= Semi (za <> ya) (zb <> act za yb) -- by ordered semigroup observation above (or split into = and < cases to handle the strict ordered semigroup proof below)
-- = Semi za zb <> Semi ya yb -- definition (backwards)
-- @
--
-- Similarly,
-- Given this assumption, by strict ordered semigroup ordering @xa = ya@ implies @xa <> za = ya <> za@
-- By ordered semigroup (act za xb <= act za yb) implies (zb <> act za xb <= zb <> act za yb)

-- xa <= ya ==> xa <> act xa zb <= ya <> act xa zb by ordered semigroup
-- similarly act xa zb < act ya zb by ordered monotone action
--
-- @
-- Semi xa xb <> Semi za zb
-- = Semi (xa <> za) (xa <> act xa zb) -- definition
-- = Semi (ya <> za) (xa <> act xa zb) -- xa = ya assumption, and @a@ is a strictly ordered semigroup
-- <= Semi (ya <> za) (ya <> act xa zb) -- strict ordered semigroup ordering
-- <= Semi (ya <> za) (ya <> act ya zb) --
--
-- Otherwise, xa < ya:
--
-- By the strict ordered semigroup: (xa < ya) implies (za <> xa < za <> ya)
-- By lexicographical ordering, (za <> xa) < (za <> ya) implies forall u and v, (Semi (za <> xa) u < Semi (za <> yb) v)
--
-- @
-- Semi za zb <> Semi xa xb
-- = Semi (za <> xa) (zb <> act za xb) -- definition
-- < Semi (za <> ya) (zb <> act za yb) -- by lexicographical argument above, u = zb <> act za xb, v = zb <> act za yb
-- = Semi za zb <> Semi ya yb -- definition (backwards)
-- @
--
-- ... left off because my plane flight ended.
instance (DistributiveAction a b, StrictlyOrderedSemigroup a, OrderedSemigroup b, StrictlyMonotoneAction a b) => OrderedSemigroup (Semi a b) where

-- | Refinement of the proof for 'OrderedSemigroup'
instance (DistributiveAction a b, StrictlyOrderedSemigroup a, StrictlyOrderedSemigroup b, StrictlyMonotoneAction a b) => StrictlyOrderedSemigroup (Semi a b) where

-- |
-- @
-- mempty <> Semi a b
-- = Semi mempty mempty <> Semi a b -- definition
-- = Semi (mempty <> a) (mempty <> act mempty b) -- definition
-- = Semi a (mempty <> act mempty b) -- left identity
-- = Semi a (act mempty b) -- left identity
-- = Semi a b -- monoid action
-- @
--
-- @
-- Semi a b <> mempty
-- = Semi a b <> Semi mempty mempty -- definition
-- = Semi (a <> mempty) (b <> act a mempty) -- definition
-- = Semi a (b <> act a mempty) -- right identity
-- = Semi a (b <> mempty) -- unitary action
-- = Semi a b -- right identity
-- @
instance (MonoidAction a b, UnitaryAction a b) => Monoid (Semi a b) where
  mempty = Semi mempty mempty
  mappend = (<>)

-- | The semidirect product of groups is a group, given a unitary action between them
--
-- @
-- inv (Semi a b) <> Semi a b
--   = Semi (inv a <> a) (act (inv a) (inv b) <> act (inv a) b) -- definition
--   = Semi mempty (act (inv a) (inv b) <> act (inv a) b) -- left inverse law
--   = Semi mempty (act (inv a) (inv b <> b)) -- distributive action
--   = Semi mempty (act (inv a) mempty) -- left inverse law
--   = Semi mempty mempty -- unitary action
--   = mempty -- definition
-- @
--
-- @
-- Semi a b <> inv (Semi a b)
--   = Semi (a <> inv a) (act (inv a) b <> act (inv a) (inv b)) -- definition
--   = Semi mempty (act (inv a) b <> act (inv a) (inv b)) -- right inverse law
--   = Semi mempty (act (inv a) (b <> inv b)) -- distributive action
--   = Semi mempty (act (inv a) mempty) -- right inverse law
--   = Semi mempty mempty -- unitary action
--   = mempty -- definition
-- @
--
-- @
-- inv (inv (Semi a b))
-- = inv (Semi (inv a) (inv a `act` inv b)) -- definition
-- = Semi (inv (inv a)) (inv (inv a) `act` inv (inv a `act` inv b)) -- definition
-- = Semi a (inv (inv a) `act` inv (inv a `act` inv b)) -- inverse inverse
-- = Semi a (a `act` inv (inv a `act` inv b)) -- inverse inverse
-- = Semi a (a `act` inv a `act` (inv (inv b))) -- by inverse action lemma for unitary actions
-- = Semi a (a `act` inv a `act` b) -- inverse inverse
-- = Semi a (act (a <> inv a) b) -- semigroup action
-- = Semi a (act mempty b) -- right inverse
-- = Semi a b -- unitary action
-- @
instance (Group a, Group b, MonoidAction a b, UnitaryAction a b) => RegularSemigroup (Semi a b) where
  inv (Semi a b) = Semi (inv a) (inv a `act` inv b)
instance (Group a, Group b, MonoidAction a b, UnitaryAction a b) => InverseSemigroup (Semi a b)
instance (Group a, Group b, MonoidAction a b, UnitaryAction a b) => Group (Semi a b)

-- | @
-- act (a <> a') (Semi b c)
-- = Semi (act (a <> a') b) (act (a <> a') c) -- definition
-- = Semi (act a (act a' b)) (act (a <> a') c)) -- semigroup action
-- = Semi (act a (act a' b)) (act a (act a' c)) -- semigroup action
-- = act a (Semi (act a' b) (act a' c)) -- definition (backwards)
-- = act a (act a' (Semi b c)) -- definition (backwards)
-- @
instance (SemigroupAction a b, SemigroupAction a c) => SemigroupAction a (Semi b c) where
  act a (Semi b c) = Semi (act a b) (act a c)

-- | @
-- act mempty (Semi b c)
-- = Semi (act mempty b) (act mempty c) -- definition
-- = Semi b (act mempty c) -- monoid action
-- = Semi b c -- monoid action
-- @
instance (MonoidAction a b, MonoidAction a c) => MonoidAction a (Semi b c) where

-- | @
-- act a (Semi b c <> Semi d e)
-- = act a (Semi (b <> d) (c <> act b e)) -- definition of <>
-- = Semi (act a (b <> d)) (act a (c <> act b e)) -- definition of act
-- = Semi (act a b <> act a d) (act a (c <> act b e)) -- distributive action of a over b
-- = Semi (act a b <> act a d) (act a c <> act a (act b e)) -- distributive action of a over c
-- = Semi (act a b <> act a d) (act a c <> act (act a b) (act a e)) -- distributive action action of a on b and c
-- = Semi (act a b) (act a c) <> Semi (act a d) (act a e) -- definition (backwards)
-- = act a (Semi b c) <> act a (Semi d e) -- definition (backwards)
-- @
instance DistributiveActionAction a b c => DistributiveAction a (Semi b c)
