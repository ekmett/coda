{-# language BangPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Syntax.Dyck
  (
  -- Dyck language monoid
    Dyck(..), Opening(..), Closing(..)
  , token
  , layoutToken
  , close
  , open
  , spine
  -- Support for multiple languages
  , Lexer(lex)
  ) where

import Control.Comonad
import Control.Lens
import Coda.Syntax.Mode
import Coda.Syntax.Rich
import Coda.Syntax.Rope
import Coda.Relative.Cat as Cat
import Coda.Relative.Class
import Coda.Relative.Rev
import Data.Default
import Data.Semigroup
import Data.String
import Data.Text
import GHC.Generics
import Prelude hiding (lex)

--------------------------------------------------------------------------------
-- Dyck Language
--------------------------------------------------------------------------------

data Opening a
  = Opening {-# unpack #-} !(LocatedPair a) !(Cat a)

data Closing a
  = Closing !(Cat a) {-# unpack #-} !(LocatedPair a)

-- | @Dyck l ms r s k e@
--
-- @k@ indicates if the last token was a layout keyword, and if so can provide a
-- numerical indicator as to which one. 0 means either no it wasn't or that we
-- haven't seen a
data Dyck a
  = Dyck
    !(Cat (Closing a))
    !(Cat a)
    !(Rev Cat (Opening a))
    !(Cat a)
    {-# unpack #-} !Mode
    !(Cat (MismatchError a)) -- errors


deriving instance Generic (Opening a)
deriving instance Generic (Closing a)
deriving instance Generic (Dyck a)

deriving instance (Relative a, Show (Pair a), Show a) => Show (Opening a)
deriving instance (Relative a, Show (Pair a), Show a) => Show (Closing a)
deriving instance (Relative a, Show (Pair a), Show a) => Show (Dyck a)

deriving instance (Relative a, Ord (Pair a), Ord a) => Ord (Opening a)
deriving instance (Relative a, Ord (Pair a), Ord a) => Ord (Closing a)
deriving instance (Relative a, Ord (Pair a), Ord a) => Ord (Dyck a)

deriving instance (Relative a, Eq (Pair a), Eq a) => Eq (Opening a)
deriving instance (Relative a, Eq (Pair a), Eq a) => Eq (Closing a)
deriving instance (Relative a, Eq (Pair a), Eq a) => Eq (Dyck a)

deriving instance (Relative a, Read (Pair a), Read a) => Read (Opening a)
deriving instance (Relative a, Read (Pair a), Read a) => Read (Closing a)
deriving instance (Relative a, Read (Pair a), Read a) => Read (Dyck a)

instance Relative a => Relative (Opening a) where
  rel d (Opening p xs) = Opening (rel d p) (rel d xs)

instance Relative a => Relative (Closing a) where
  rel d (Closing xs q) = Closing (rel d xs) (rel d q)

instance Relative a => Relative (Dyck a) where
  rel d (Dyck l ms r s k e) = Dyck (rel d l) (rel d ms) (rel d r) (rel d s) k (rel d e)

-- | O(1)
token :: Relative a => Dyck a -> a -> Dyck a
token (Dyck l ms r s _ e) a = Dyck l ms r (snocCat s a) 0 e

layoutToken :: Relative a => Dyck a -> Mode -> a -> Dyck a
layoutToken (Dyck l ms r s _ e) i a = Dyck l ms r (snocCat s a) i e

-- | O(1)
close :: (Eq (Pair a), AsRich a a, Relative a) => Dyck a -> LocatedPair a -> Dyck a
close (Dyck l ms (r' :> Opening dp rs) s _ e) dq
 | extract dp == extract dq  = Dyck l ms r' (Cat.singleton $ Rich $ Nested dp (rs<>s)) 0 e
 | !f <- MismatchError dp dq = Dyck l ms r' (Cat.singleton $ Rich $ Mismatch f (rs<>s)) 0 (snocCat e f)
close (Dyck l ms _ s _ e) dq = Dyck (snocCat l $ Closing (ms <> s) dq) mempty mempty mempty 0 e

-- | O(1)
open :: Relative a => Dyck a -> LocatedPair a -> Dyck a
open (Dyck l m (r' :> Opening dp rs) s _ e) dq = Dyck l m ((r' :> Opening dp (rs<>s)) :> Opening dq mempty) s 0 e
open (Dyck l m _ s _ e) dp = Dyck l (m<>s) (Rev $ Cat.singleton $ Opening dp mempty) mempty 0 e

instance Default (Dyck a) where
  def = Dyck def def def def def def

-- | O(k) in the number of canceled contexts
--
-- Note: positions are not shifted, so you'll need to use this inside a semi-direct product with Delta.
instance (Eq (Pair a), AsRich a a, Relative a) => Semigroup (Dyck a) where
  Dyck l0 m0 r0 s0 k0 e0 <> Dyck l1 m1 r1 s1 k1 e1 = go l0 m0 r0 s0 e0 l1 m1 r1 s1 (k0 <> k1) e1 where
    go l2 m2 (r2' :> Opening dp xs) s2 e2 (Closing ys dq :< l3') m3 r3 s3 k3 e3
      | extract dp == extract dq  = go l2 m2 r2' (Cat.singleton $ Rich $ Nested dp (xs<>s2<>ys))  e2             l3' m3 r3 s3 k3 e3
      | !f <- MismatchError dp dq = go l2 m2 r2' (Cat.singleton $ Rich $ Mismatch f (xs<>s2<>ys)) (snocCat e2 f) l3' m3 r3 s3 k3 e3
    go l2 m2 (r2' :> Opening dp xs) s2 e2 _ m3 r3 s3 k3 e3 = Dyck l2 m2 ((r2' :> Opening dp (xs<>s2<>m3))<>r3) s3 k3 (e2<>e3)
    go l2 m2 _ s2 e2 (Closing xs dp :< l3') m3 r3 s3 k3 e3 = Dyck (l2<>(Closing (m2<>s2<>xs) dp :< l3')) m3 r3 s3 k3 (e2<>e3)
    go l2 m2 _ s2 e2 _ m3 r3 s3 k3 e3 = Dyck l2 (m2<>s2<>m3) r3 s3 k3 (e2<>e3)

instance (Eq (Pair a), AsRich a a, Relative a) => Monoid (Dyck a) where
  mempty = Dyck mempty mempty mempty mempty mempty mempty
  mappend = (<>)
  {-# inline mappend #-}

instance (Eq (Pair a), AsRich a a, Relative a) => RelativeMonoid (Dyck a)

-- convert a dyck language skeleton to a set of tokens (including unmatched closings and openings)
spine :: (AsRich a a, Relative a) => Dyck a -> Cat a
spine (Dyck l0 ms0 r0 s0 _ _) = go1 l0 <> ms0 <> go2 r0 <> s0 where
  go1 (Closing xs dp :< l') = xs <> (Rich (UnmatchedClosing dp) :< go1 l')
  go1 _ = mempty
  go2 (r' :> Opening dp ys) = go2 r' <> (Rich (UnmatchedOpening dp) :< ys)
  go2 _ = mempty
{-# inline spine #-}

class Lexer a where
  lex :: Text -> Dyck a

instance Lexer a => FromText (Dyck a) where
  fromText = lex

instance Lexer a => IsString (Dyck a) where
  fromString = fromText . fromString

instance HasMode (Dyck a) where
  mode (Dyck _ _ _ _ k _) = k

