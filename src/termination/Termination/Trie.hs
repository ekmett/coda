{-# language CPP #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}

-- |
--
-- Well-Quasi-Orders using tries

module Termination.Trie
  ( Trie(..)
  , runTrie
  , finite
  , finiteOrd
  , finiteHash
  , history
  ) where

import Data.Functor
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Coerce
import Data.Map as Map
import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Proxy
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import Data.Void

import Termination.History

-- | A well quasi-order: A reflexive, transitive relation such that
-- and every infinite set xs has i < j such that (xs!i) <= (xs!j)
-- encoded as a procedure for maintaining 'xs' in an easily testable form

-- This is an experiment to see if we can use a trie-based encoding.
-- How to handle orders?

data Trie a where Trie :: (forall x. x -> f x) -> (forall x. a -> x -> (x -> Maybe x) -> f x -> Maybe (f x)) -> Trie a

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

newtype V a b = V [(a,b)]

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

-- side-condition: needs 'a' to be finitely enumerable and have an 'Ord' instance -- log time
finiteOrd :: Ord a => Trie a
finiteOrd = Trie (const mempty) $ \ a d k -> Map.alterF (fmap Just . k . fromMaybe d) a

atH :: (Functor f, Hashable k, Eq k) => k -> (Maybe a -> f (Maybe a)) -> HashMap k a -> f (HashMap k a)
atH k f m = f mv <&> \case
    Nothing -> maybe m (const (HashMap.delete k m)) mv
    Just v' -> HashMap.insert k v' m
  where mv = HashMap.lookup k m

finiteHash :: (Hashable a, Eq a) => Trie a
finiteHash = Trie (const mempty) $ \a d k -> atH a (fmap Just . k . fromMaybe d)

-- can I handle orders? I can't compile down to a test, can I incorporate tests as another constructor?
-- or handle a mix of test and non-test parts?

history :: Trie a -> History a
history (Trie p f) = History step (p False) where
  step xs a = f a False seen xs
