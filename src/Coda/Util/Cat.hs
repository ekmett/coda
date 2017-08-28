{-# language FlexibleContexts #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Coda.Util.Cat 
  ( Cat, null
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Semigroup

data P a = P a a
  deriving (Functor, Foldable, Traversable)

data B a
  = B1 a
  | B2 !(P a)
  deriving (Functor, Foldable, Traversable)

data Q a 
  = Q0
  | Q1 a
  | QN (B a) (Q (P a)) (B a)
  deriving (Functor, Foldable,Traversable)

snocQ :: Q a -> a -> Q a
snocQ Q0 b = Q1 b
snocQ (Q1 a) b = QN (B1 a) Q0 (B1 b)
snocQ (QN l m (B1 a)) b = QN l m (B2 (P a b))
snocQ (QN l m (B2 r)) b = QN l (snocQ m r) (B1 b)

unconsQ :: Q a -> Maybe (a, Q a)
unconsQ Q0 = Nothing
unconsQ (Q1 a) = Just (a, Q1 a)
unconsQ (QN (B2 (P a b)) m r) = Just (a, QN (B1 b) m r)
unconsQ (QN (B1 a) m r) = Just (a, q') where
  q' = case unconsQ m of
    Just (l, m') -> QN (B2 l) m' r
    Nothing -> case r of
      B1 x -> Q1 x
      B2 (P x y) -> QN (B1 x) Q0 (B1 y)

-- | An Okasaki catenable list
-- 
-- mappend, cons, snoc, uncons are all O(1)
data Cat a
  = E
  | C a (Q (Cat a))
  deriving (Functor, Traversable)

instance Foldable Cat where
  null E = True
  null _ = False
  foldMap _ E = mempty
  foldMap f (C a q) = f a `mappend` foldMap (foldMap f) q

instance Applicative Cat where
  pure a = C a Q0
  fs <*> as = foldMap (<$> as) fs

instance Monad Cat where
  m >>= f = foldMap f m

instance Alternative Cat where
  empty = E
  E <|> xs = xs
  xs <|> E = xs
  xs <|> ys = link xs ys

instance MonadPlus Cat where
  mzero = empty
  mplus = (<|>)

instance Monoid (Cat a) where
  mempty = empty
  mappend = (<|>)

instance Semigroup (Cat a) where
  (<>) = (<|>)

link :: Cat a -> Cat a -> Cat a
link (C x q) s = C x (snocQ q s)
link E s = s -- never happens

linkAll :: Cat a -> Q (Cat a) -> Cat a
linkAll t q' = case unconsQ q' of
  Nothing -> t
  Just (x, q'') -> link t (linkAll x q'') 

instance Cons (Cat a) (Cat b) a b where
  _Cons = prism embed project where
    embed :: (a, Cat a) -> Cat a
    embed (a, as) = C a Q0 <> as
    project :: Cat a -> Either (Cat b) (a, Cat a)
    project E       = Left E
    project (C a q) = Right (a, rest) where
      rest = case unconsQ q of
        Nothing -> E
        Just (t, q') -> linkAll t q'
