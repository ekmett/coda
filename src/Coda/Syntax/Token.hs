{-# language BangPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language TypeFamilies #-}

module Coda.Syntax.Token 
  ( Pair(..)
  , pair
  , TokenType(..)
  , MismatchError(..)
  , Token(..)
  , Opening(..)
  , Closing(..)
  , Rev(..)
  , _Rev
  , Dyck(..)
  , token
  , close
  , open
  , spine
  ) where

import Control.Exception
import Control.Lens
import Coda.Relative.Cat as Cat
import Coda.Relative.Class
import Coda.Relative.Delta
import Data.Data
import Data.Ix
import Data.Semigroup
import Data.Text
import GHC.Generics hiding (from)

-- paired brackets

data Pair = Brace | Bracket | Paren
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

data TokenType
  = LSpecial
  | LReservedId
  | LQVarId
  | LQConId
  | LVarId
  | LConId
  | LQVarSym
  | LQConSym
  | LVarSym
  | LConSym
  | LInteger
  | LFloat
  | LChar
  | LString
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

pair :: Char -> Pair
pair '(' = Paren
pair ')' = Paren
pair '[' = Bracket
pair ']' = Bracket
pair '{' = Brace
pair '}' = Brace
pair _ = error "bad pair"

data MismatchError = MismatchError !Delta !Delta !Pair !Pair
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance Exception MismatchError

instance Relative MismatchError where
  rel d (MismatchError d' d'' p q) = MismatchError (d+d') (d+d'') p q

data Token
  = Token !Delta !TokenType !Text
  | Nested !Delta !Pair !(Cat Token)
  | Mismatch !MismatchError !(Cat Token)
  | UnmatchedOpening !Delta !Pair
  | UnmatchedClosing !Delta !Pair
  deriving (Eq,Ord,Show,Read)

instance Relative Token where
  rel 0 xs = xs
  rel d (Token d' t x)   = Token (d+d') t x
  rel d (Nested d' p xs) = Nested (d+d') p (rel d xs)
  rel d (Mismatch m q)   = Mismatch (rel d m) (rel d q)
  rel d (UnmatchedOpening d' p) = UnmatchedOpening (d+d') p
  rel d (UnmatchedClosing d' p) = UnmatchedClosing (d+d') p

data Opening = Opening !Delta !Pair !(Cat Token)
  deriving (Eq,Ord,Show,Read)

instance Relative Opening where
  rel d (Opening d' p xs ) = Opening (d+d') p (rel d xs)

data Closing = Closing !(Cat Token) !Delta !Pair
  deriving (Eq,Ord,Show,Read)

instance Relative Closing where
  rel d (Closing xs d' p) = Closing (rel d xs) (d+d') p

newtype Rev f a = Rev { runRev :: f a }
  deriving (Eq,Ord,Show,Read)

makePrisms ''Rev
makeWrapped ''Rev

instance Snoc (f a) (f b) a b => Cons (Rev f a) (Rev f b) a b where
  _Cons = _Wrapped._Snoc.swapped.mapping (from _Wrapped)

instance Cons (f a) (f b) a b => Snoc (Rev f a) (Rev f b) a b where
  _Snoc = _Wrapped._Cons.mapping (from _Wrapped).swapped

instance Semigroup (f a) => Semigroup (Rev f a) where
  Rev a <> Rev b = Rev (b <> a)

instance Monoid (f a) => Monoid (Rev f a) where
  mempty = Rev mempty
  mappend (Rev a) (Rev b) = Rev (mappend b a)

instance Relative (f a) => Relative (Rev f a) where
  rel d (Rev m) = Rev (rel d m)

data Dyck = Dyck !(Cat Closing) !(Cat Token) !(Rev Cat Opening) !(Cat Token) !(Cat MismatchError)
  deriving (Eq,Ord,Show,Read)

instance Relative Dyck where
  rel 0 xs = xs
  rel d (Dyck l ms r s e) = Dyck (rel d l) (rel d ms) (rel d r) (rel d s) (rel d e)

--------------------------------------------------------------------------------
-- parsing within a line
--------------------------------------------------------------------------------

-- | O(1)
token :: Dyck -> Token -> Dyck
token (Dyck l ms r s e) a = Dyck l ms r (snocCat s a) e

-- | O(1)
close :: Dyck -> Delta -> Pair -> Dyck
close (Dyck l ms (r' :> Opening d p rs) s e) d' q
 | p == q                       = Dyck l ms r' (Cat.singleton $ Nested d p (rs <> s)) e
 | !f <- MismatchError d d' p q = Dyck l ms r' (Cat.singleton $ Mismatch f (rs <> s)) (snocCat e f)
close (Dyck l ms _ s e) d q = Dyck (snocCat l $ Closing (ms <> s) d q) mempty mempty mempty e

-- | O(1)
open :: Dyck -> Delta -> Pair -> Dyck
open (Dyck l m (r' :> Opening d p rs) s e) d' q = Dyck l m ((r' :> Opening d p (rs <> s)) :> Opening d' q mempty) s e
open (Dyck l m _ s e) d p = Dyck l (m <> s) (Rev $ Cat.singleton $ Opening d p mempty) mempty e

--------------------------------------------------------------------------------
-- parsing across lines
--------------------------------------------------------------------------------

-- | O(k) in the number of canceled contexts
instance Semigroup Dyck where
  Dyck l0 m0 r0 s0 e0 <> Dyck l1 m1 r1 s1 e1 = go l0 m0 r0 s0 e0 l1 m1 r1 s1 e1 where
    go l2 m2 (r2' :> Opening d p xs) s2 e2 (Closing ys d' q :< l3') m3 r3 s3 e3
      | p == q = go l2 m2 r2' (Cat.singleton $ Nested d p (xs <> s2 <> ys)) e2 l3' m3 r3 s3 e3
      | !f <- MismatchError d d' p q = go l2 m2 r2' (Cat.singleton $ Mismatch f (xs <> s2 <> ys)) (snocCat e2 f) l3' m3 r3 s3 e3
    go l2 m2 (r2' :> Opening d p xs) s2 e2 _ m3 r3 s3 e3 = Dyck l2 m2 ((r2' :> Opening d p (xs <> s2 <> m3)) <> r3) s3 (e2 <> e3)
    go l2 m2 _ s2 e2 (Closing xs d p :< l3') m3 r3 s3 e3 = Dyck (l2 <> (Closing (m2 <> s2 <> xs) d p :< l3')) m3 r3 s3 (e2 <> e3)
    go l2 m2 _ s2 e2 _ m3 r3 s3 e3 = Dyck l2 (m2 <> s2 <> m3) r3 s3 (e2 <> e3)
       
instance Monoid Dyck where
  mempty = Dyck mempty mempty mempty mempty mempty
  mappend = (<>)
  {-# inline mappend #-}

-- convert a dyck language skeleton to a set of tokens (including unmatched closings and openings)
spine :: Dyck -> Cat Token
spine (Dyck l0 ms0 r0 s0 _) = go1 l0 <> ms0 <> go2 r0 <> s0 where
  go1 (Closing xs d p :< l') = xs <> (UnmatchedClosing d p :< go1 l')
  go1 _ = mempty
  go2 (r' :> Opening d p ys) = go2 r' <> (UnmatchedOpening d p :< ys)
  go2 _ = mempty
{-# inline spine #-}
