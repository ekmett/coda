{-# language GADTs #-}
{-# language RankNTypes #-}

-- |
--
-- Well-Quasi-Orders using tries

module Coda.Termination.Trie 
  ( Trie(..)
  , runTrie
  , finite
  , ord
  , history
  ) where

import Coda.Termination.History
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Coerce
import Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Semigroup
import Data.Void

-- | A well quasi-order: A reflexive, transitive relation such that 
-- and every infinite set xs has i < j such that (xs!i) <= (xs!j)
-- encoded as a procedure for maintaining 'xs' in an easily testable form

data Trie a where 
   Trie :: (forall x. x -> f x) -> (forall x. a -> x -> (x -> Maybe x) -> f x -> Maybe (f x)) -> Trie a

instance Contravariant Trie where
  contramap f (Trie h g) = Trie h (g . f)

instance Divisible Trie where
  conquer = Trie Identity (\_ _ -> coerce :: (x -> Maybe x) -> Identity x -> Maybe (Identity x))
  divide f (Trie p g) (Trie q h) = Trie (Compose . p . q) $ \a d k -> case f a of
    (b, c) -> fmap Compose . g b (q d) (h c d k) . getCompose

instance Decidable Trie where
  lose f = Trie (const Proxy) (absurd . f)
  choose f (Trie p g) (Trie q h) = Trie (\a -> Pair (p a) (q a)) $ \a d k (Pair x y) -> case f a of
    Left b -> (`Pair` y) <$> g b d k x
    Right c -> Pair x <$> h c d k y

instance Semigroup (Trie a) where
  Trie p g <> Trie q h = Trie (Compose . p . q) $ \a d k -> fmap Compose . g a (q d) (h a d k) . getCompose

instance Monoid (Trie a) where
  mempty = conquer
  mappend = (<>)

seen :: Bool -> Maybe Bool
seen True = Nothing
seen False = Just True

runTrie :: Trie a -> a -> a -> Bool
runTrie (Trie p f) a b = isJust $ f a False seen (p False) >>= f b False seen 

data V a b = V [(a,b)]

-- side-condition: needs 'a' to be finitely enumerable -- linear time
finite :: Eq a => Trie a
finite = Trie (const $ V []) $ \a d k (V xs) -> fini a xs $ step a d k xs where
  fini :: a -> [(a,x)] -> Either (Maybe x) [(a,x)] -> Maybe (V a x)
  fini _ _  (Left Nothing)   = Nothing
  fini a xs (Left (Just d')) = Just $ V $ (a,d'):xs
  fini _ _  (Right ys)       = Just $ V ys
  step :: Eq a => a -> x -> (x -> Maybe x) -> [(a,x)] -> Either (Maybe x) [(a,x)]
  step _ d k [] = Left (k d)
  step a d k ((b,x):xs) 
    | a /= b = ((b,x):) <$> step a d k xs
    | otherwise = case k x of
      Nothing -> Left Nothing
      Just x' -> Right ((b,x'):xs)

-- side-condition: well-order -- log time
ord :: Ord a => Trie a
ord = Trie (const mempty) $ \ a d k -> alterF (fmap Just . k . fromMaybe d) a

history :: Trie a -> History a
history (Trie p f) = History step (p False) where
  step xs a = f a False seen xs
