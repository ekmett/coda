-- | Based on "Implementing Explicit and Finding Implicit Sharing in Embedded DSLs"
-- by Oleg Kiselyov, then perverted for a completely antithetical purpose by
-- the BDD module
module Data.Bimap 
  ( Bimap(..)
  , Id
  , lookupKey
  , lookupVal
  , insert
  , insertR
  , empty
  ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List.Skew as Skew

type Id = Int

data Bimap a = Bimap !Id !(HashMap a Id) !(Skew a)

lookupKey :: (Hashable a, Eq a) => a -> Bimap a -> Maybe Id
lookupKey a (Bimap _ m _) = HashMap.lookup a m

lookupVal :: Id -> Bimap a -> a
lookupVal i (Bimap k _ n) = Skew.index n (k - i)

-- for use with modify
insert :: (Hashable a, Eq a) => a -> Bimap a -> (Id, Bimap a)
insert a b@(Bimap i m n) = case HashMap.lookup a m of
  Nothing -> (i, Bimap (i+1) (HashMap.insert a i m) (Cons a n))
  Just j -> (j, b)

-- swapped for convenient use with atomicModifyIORef
insertR :: (Hashable a, Eq a) => a -> Bimap a -> (Bimap a, Id)
insertR a b@(Bimap i m n) = case HashMap.lookup a m of
  Nothing -> (Bimap (i+1) (HashMap.insert a i m) (Cons a n), i)
  Just j -> (b, j)

-- node ids start at 1, so we can use negated node ids
empty :: Bimap a
empty = Bimap 1 HashMap.empty Skew.Nil
