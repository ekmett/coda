{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Dyck
  (
  -- Dyck language monoid
    Dyck(..)
  , Opening(..)
  , Closing(..)
  , LocatedPair
  , MismatchError(..)
  , token
  , layoutToken
  , close
  , open
  , spine
  , dyckLayoutMode
  , dyckMismatchErrors
  , boring
  ) where


import Control.Comonad
import Control.Exception
import Control.Lens
import Data.Default
import Data.Semigroup
import GHC.Generics
import Prelude

import Relative.Cat as Cat
import Relative.Class
import Relative.Located
import Rev

import Token

--------------------------------------------------------------------------------
-- Dyck Language
--------------------------------------------------------------------------------

type LocatedPair = Located Pair

data MismatchError = MismatchError {-# unpack #-} !LocatedPair {-# unpack #-} !LocatedPair
  deriving (Show, Read, Eq, Ord)

instance Exception MismatchError

instance Relative MismatchError where
  rel 0 xs = xs
  rel d (MismatchError l r) = MismatchError (rel d l) (rel d r)

data Opening = Opening {-# unpack #-} !LocatedPair !(Cat Token)
  deriving (Generic, Show, Eq, Ord, Read)

data Closing = Closing !(Cat Token) {-# unpack #-} !LocatedPair
  deriving (Generic, Show, Eq, Ord, Read)

-- | @Dyck l ms r s k e@
--
-- @k@ indicates if the last token was a layout keyword, and if so can provide a
-- numerical indicator as to which one. 0 means either no it wasn't or that we
-- haven't seen a
data Dyck
  = Dyck
    !(Cat Closing)
    !(Cat Token)
    !(Rev Cat Opening)
    !(Cat Token)
    !LayoutMode
    !(Cat MismatchError) -- errors
  deriving (Generic, Show, Eq, Ord, Read)

boring :: Dyck -> Bool
boring = views dyckLayoutMode (def ==)

dyckLayoutMode :: Lens' Dyck LayoutMode
dyckLayoutMode f (Dyck l ms r s k e) = f k <&> \k' -> Dyck l ms r s k' e

dyckMismatchErrors :: Lens' Dyck (Cat MismatchError)
dyckMismatchErrors f (Dyck l ms r s k e) = Dyck l ms r s k <$> f e

instance AsEmpty Dyck where
  _Empty = prism (const def) $ \case
    Dyck Empty Empty (Rev Empty) Empty _ Empty -> Right ()
    x -> Left x

instance Relative Opening where
  rel d (Opening p xs) = Opening (rel d p) (rel d xs)

instance Relative Closing where
  rel d (Closing xs q) = Closing (rel d xs) (rel d q)

instance Relative Dyck where
  rel 0 xs = xs
  rel d (Dyck l ms r s k e) = Dyck (rel d l) (rel d ms) (rel d r) (rel d s) k (rel d e)

-- | O(1)
token :: Dyck -> Token -> Dyck
token (Dyck l ms r s _ e) a = Dyck l ms r (snocCat s a) def e

layoutToken :: Dyck -> LayoutMode -> Token -> Dyck
layoutToken (Dyck l ms r s _ e) i a = Dyck l ms r (snocCat s a) i e

-- | O(1)
close :: Dyck -> Located Pair -> Dyck
close (Dyck l ms (r' :> Opening dp@(Located lp p) rs) s _ e) dq@(Located lq q)
 | p == q  = Dyck l ms r' (Cat.singleton $ nested p lp (rs<>s) lq) def e
 | otherwise = Dyck l ms r' (Cat.singleton $ mismatch dp dq (rs<>s)) def (snocCat e $! MismatchError dp dq)
close (Dyck l ms _ s _ e) dq = Dyck (snocCat l $ Closing (ms <> s) dq) mempty mempty mempty def e

-- | O(1)
open :: Dyck -> Located Pair -> Dyck
open (Dyck l m (r' :> Opening dp rs) s _ e) dq = Dyck l m ((r' :> Opening dp (rs<>s)) :> Opening dq mempty) s def e
open (Dyck l m _ s _ e) dp = Dyck l (m<>s) (Rev $ Cat.singleton $ Opening dp mempty) mempty def e

instance Default Dyck where
  def = Dyck def def def def def def

-- | O(k) in the number of canceled contexts
--
-- Note: positions are not shifted, so you'll need to use this inside a semi-direct product with Delta.
instance Semigroup Dyck where
  m <> Dyck Empty Empty Empty Empty _ Empty = m
  Dyck l0 m0 r0 s0 _ e0 <> Dyck l1 m1 r1 s1 k1 e1 = go l0 m0 r0 s0 e0 l1 m1 r1 s1 k1 e1 where
    go l2 m2 (r2' :> Opening dp@(Located lp p) xs) s2 e2 (Closing ys dq@(Located lq q) :< l3') m3 r3 s3 k3 e3
      | p == q    = go l2 m2 r2' (Cat.singleton $ nested p lp (xs<>s2<>ys) lq) e2 l3' m3 r3 s3 k3 e3
      | otherwise = go l2 m2 r2' (Cat.singleton $ mismatch dp dq (xs<>s2<>ys)) (snocCat e2 $! MismatchError dp dq) l3' m3 r3 s3 k3 e3
    go l2 m2 (r2' :> Opening dp xs) s2 e2 _ m3 r3 s3 k3 e3 = Dyck l2 m2 ((r2' :> Opening dp (xs<>s2<>m3))<>r3) s3 k3 (e2<>e3)
    go l2 m2 _ s2 e2 (Closing xs dp :< l3') m3 r3 s3 k3 e3 = Dyck (l2<>(Closing (m2<>s2<>xs) dp :< l3')) m3 r3 s3 k3 (e2<>e3)
    go l2 m2 _ s2 e2 _ m3 r3 s3 k3 e3 = Dyck l2 (m2<>s2<>m3) r3 s3 k3 (e2<>e3)

instance Monoid Dyck where
  mempty = Dyck mempty mempty mempty mempty def mempty
  mappend = (<>)
  {-# inline mappend #-}

instance RelativeSemigroup Dyck

instance RelativeMonoid Dyck

-- convert a dyck language skeleton to a set of tokens (including unmatched closings and openings)
spine :: Dyck -> Cat Token
spine (Dyck l0 ms0 r0 s0 _ _) = go1 l0 <> ms0 <> go2 r0 <> s0 where
  go1 (Closing xs dp :< l') = xs <> (unmatchedClosing dp :< go1 l')
  go1 _ = mempty
  go2 (r' :> Opening dp ys) = go2 r' <> (unmatchedOpening dp :< ys)
  go2 _ = mempty
{-# inline spine #-}
