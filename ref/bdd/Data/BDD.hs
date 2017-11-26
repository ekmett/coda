{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveAnyClass #-}
{-# language RoleAnnotations #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language PatternGuards #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language StrictData #-}
{-# options_ghc -funbox-strict-fields #-}

module Data.BDD
  ( -- * ROBDDs
    BDD(Zero, One, BDD, ROBDD)
    -- * combinators
  , ite
  , neg
  , and
  , or
  , xor
  , bool
  , var
  , liftB2
    -- * functions of two arguments
  , Fun(..)
  , fun, table
    -- * memo management
  , reifyCache
  , copy_     -- copy
  , copy      -- relabel and copy
  , copy'     -- relabel and copy (in the same cache)
  , copyMono  -- relabel monotonically and copy
  , copyMono' -- relabel monotonically and copy (in the same cache)
    -- * computations
  , sat
  , tautology
    -- * enumerating solutions
  , Binding(..)
  , sats
    -- * observations
  , Data.BDD.size
  , node
  , Node(..)
  ) where

import Control.Monad.Trans.State.Strict
import Data.Bimap as Bimap
import Data.Coerce
import Data.Data
import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Reflection
import Data.Semigroup
import Data.Set as Set
import Data.Sequence as Seq
import Data.Tuple (swap)
import GHC.Arr
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (or, and)

type Var = Int

type NodeId = Id

data Node = F | T | Node NodeId Var Node Node
  deriving (Show,Data,Generic)

instance Eq Node where
  F == F = True
  T == T = True
  Node i _ _ _ == Node j _ _ _ = i == j
  _ == _ = False

instance Ord Node where
  F `compare` F = EQ
  F `compare` _ = LT
  T `compare` F = GT
  T `compare` T = EQ
  T `compare` Node{} = LT
  Node i _ _ _ `compare` Node j _ _ _ = compare i j
  Node{} `compare` _ = GT

instance Hashable Node where
  hash F = 0
  hash T = maxBound
  hash (Node i _ _ _) = i

data Key = Key Var Node Node
  deriving (Eq,Ord,Show,Data,Generic,Hashable)

data ITE = ITE Node Node Node
  deriving (Eq,Ord,Show,Data,Generic,Hashable)

newtype BDD s = D Node
  deriving (Eq,Ord,Show,Data,Generic,Hashable)

node :: BDD s -> Node
node (D n) = n

type Memo = HashMap ITE Node -- cached ite results

type DAG = Bimap Key

data Cache = Cache
  { getCache :: IORef DAG
  , getMemo :: IORef Memo
  } deriving Eq

modifyCache :: forall s r proxy. Reifies s Cache => proxy s -> (DAG -> (DAG, r)) -> IO r
modifyCache _ = atomicModifyIORef $ getCache $ reflect (Proxy :: Proxy s)

modifyMemo :: forall s r proxy. Reifies s Cache => proxy s -> (Memo -> (Memo, r)) -> IO r
modifyMemo _ = atomicModifyIORef $ getMemo $ reflect (Proxy :: Proxy s)

-- this node is allowed as a child of a 'hi' branch for a BDD node
okhi :: Node -> Bool
okhi F = False
okhi (Node i _ _ _) = i > 0
okhi _ = True

nodeId :: forall s proxy. Reifies s Cache => proxy s -> Var -> Node -> Node -> NodeId
nodeId _ v l r = unsafePerformIO $ modifyCache (Proxy :: Proxy s) $ Bimap.insertR $ Key v l r

bdd :: forall s. Reifies s Cache => Var -> BDD s -> BDD s -> BDD s
bdd v (D l) (D r)
  | l == r = D l
  | okhi r = D (Node (nodeId (Proxy :: Proxy s) v l r) v l r)
  | nl <- negNode l
  , nr <- negNode r = D (Node (- nodeId (Proxy :: Proxy s) v nl nr) v nl nr)
    
--------------------------------------------------------------------------------
-- safe user accessible BDD constructors:
--------------------------------------------------------------------------------

-- bidirectional matching and construction using the tape, censoring node ids
pattern BDD :: Reifies s Cache => Var -> BDD s -> BDD s -> BDD s
pattern BDD v l r <- D (Node _ v (D -> l) (D -> r)) where
  BDD v l r = bdd v l r

-- read only access to node ids
pattern ROBDD :: NodeId -> Var -> BDD s -> BDD s -> BDD s
pattern ROBDD i v l r <- D (Node i v (D -> l) (D -> r))

pattern Zero :: BDD s
pattern Zero = D F

pattern One :: BDD s
pattern One = D T

{-# complete Zero, One, BDD #-}
{-# complete Zero, One, ROBDD #-}
{-# complete D #-}

-- | /O(1)/, see https://www.ece.cmu.edu/~ee760/760docs/lec03.pdf Optimization: Negation Arcs
--
-- Invariants for negation arcs:
--
-- 1. no double negation (forced by only negating node ids)
--
-- 2. no negated high pointers (see hi check)
--
-- 3. no negated constants (by design of neg)
--
-- 4. no high pointers to 0
--
-- Gives constant time negation and 2x space improvement

neg :: BDD s -> BDD s
neg (D x) = D (negNode x) where

negNode :: Node -> Node
negNode F = T
negNode T = F
negNode (Node i v l r) = Node (-i) v l r

reifyCache :: (forall s. Reifies s Cache => Proxy s -> r) -> r
reifyCache f = unsafePerformIO $ do
  r <- newIORef Bimap.empty
  m <- newIORef HashMap.empty
  return $ reify (Cache r m) f

-- root decision variable
root :: BDD s -> Var
root = \case
  D (Node _ v _ _) -> v
  _ -> 0

-- "twisted" shannon decomposition to allow for negated nodes
shannon :: Var -> BDD s -> (BDD s, BDD s)
shannon u (ROBDD i v l r) | u == v = if i > 0 then (l, r) else (r, l)
shannon _ n = (n, n)

ite :: forall s. Reifies s Cache => BDD s -> BDD s -> BDD s -> BDD s
-- initial cases avoid touching the memo table
ite (D T) f _ = f
ite (D F) _ f = f
ite f (D T) (D F) = f
ite _ g h | g == h = g
-- NB: currently locks up the ite memo table for the duration
ite f0 g0 h0 = unsafePerformIO $ modifyMemo (Proxy :: Proxy s) $ swap . runState (go f0 g0 h0) where
  go :: BDD s -> BDD s -> BDD s -> State Memo (BDD s)
  go (D T) f _ = pure f
  go (D F) _ f = pure f
  go f (D T) (D F) = pure f
  go f g h
    | g == h = pure g
    | k <- coerce ITE f g h = gets (HashMap.lookup k) >>= \case
      Just r -> pure $ D r
      Nothing
        | v <- root f `max` root g `max` root h
        , (f',f'') <- shannon v f
        , (g',g'') <- shannon v g
        , (h',h'') <- shannon v h
        -> do
          r <- bdd v <$> go f'' g'' h'' <*> go f' g' h'
          r <$ modify (HashMap.insert k $ node r)

-- check satisfiability
sat :: BDD s -> Bool
sat Zero = False
sat _ = True

-- check for tautology
tautology :: BDD s -> Bool
tautology One = True
tautology _   = False

data Binding = Var := Bool deriving (Eq,Ord,Show,Read,Data,Generic,Hashable)

-- find all satisfying variable assignments
sats :: BDD s -> Seq [Binding]
sats (D n0) = evalState (go n0) HashMap.empty where
  go F = pure Seq.empty
  go T = pure (Seq.singleton [])
  go (Node i v l r) = gets (HashMap.lookup i) >>= \case
    Just x  -> pure x
    Nothing -> do
      x <- go l
      y <- go r
      if i > 0 
        then pure $ fmap ((v := False):) x <> fmap ((v := True):) y
        else pure $ fmap ((v := False):) y <> fmap ((v := True):) x

-- # of distinct nodes present in the BDD
size :: BDD s -> Int
size (D n0) = Set.size (go n0 Set.empty) where
  go F s = s
  go T s = s
  go (Node (abs -> i) _ l r) s = case Set.member i s of
    True -> s
    False -> go l $ go r $ Set.insert i s

-- all two argument functions
data Fun = TNever | TAnd | TGt | TF | TLt | TG | TXor | TOr | TNor | TXnor | TG' | TGe | TF' | TLe | TNand | TAlways
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

-- enumerate as a two argument boolean function
fun :: (Bool -> Bool -> Bool) -> Fun
fun f = toEnum 
  $ 8 * fromEnum (f False False)
  + 4 * fromEnum (f False True)
  + 2 * fromEnum (f True False)
  +     fromEnum (f True True)

table :: Reifies s Cache => Fun -> BDD s -> BDD s -> BDD s
table TNever  _ _ = Zero               -- false
table TAnd    f g = ite f g Zero       -- f && g
table TGt     f g = ite f (neg g) Zero -- f > g
table TF      f _ = f                  -- f
table TLt     f g = ite f Zero g       -- f < g
table TG      _ g = g                  -- g
table TXor    f g = ite f (neg g) g    -- xor f g
table TOr     f g = ite f One g        -- f || g
table TNor    f g = ite f Zero (neg g) -- nor f g
table TXnor   f g = ite f g (neg g)    -- xnor f g
table TG'     _ g = neg g              -- neg g
table TGe     f g = ite f One (neg g)  -- f >= g
table TF'     f _ = neg f              -- neg f
table TLe     f g = ite f g One        -- f <= g
table TNand   f g = ite f (neg g) One  -- nand f g
table TAlways _ _ = One                -- true

-- | lift boolean functions through the table e.g. @liftB2 (&&)@, @liftB2 (<=)@
liftB2 :: Reifies s Cache => (Bool -> Bool -> Bool) -> BDD s -> BDD s -> BDD s
liftB2 = table . fun

and :: Reifies s Cache => BDD s -> BDD s -> BDD s
and f g = ite f g Zero

or  :: Reifies s Cache => BDD s -> BDD s -> BDD s
or f g = ite f One g 

xor :: Reifies s Cache => BDD s -> BDD s -> BDD s
xor f g = ite f (neg g) g

bool :: Bool -> BDD s
bool False = D F
bool True = D T

var :: Reifies s Cache => Var -> BDD s
var v = bdd v Zero One

-- O(|n|) copy a BDD over to a new tape
copy_ :: forall s' s. Reifies s' Cache => BDD s -> BDD s'
copy_ (D n) = evalState (go n) HashMap.empty where
  go :: Node -> State (HashMap NodeId (BDD s')) (BDD s')
  go (Node i v l r) 
    | i > 0 = gets (HashMap.lookup i) >>= \case
      Just z -> pure z
      Nothing -> do
        z <- bdd v <$> go l <*> go r
        z <$ modify (HashMap.insert i z)
    | ni <- negate i = gets (HashMap.lookup ni) >>= \case
      Just z' -> pure $ neg z'
      Nothing -> do
        z' <- bdd v <$> go l <*> go r
        z' <$ modify (HashMap.insert ni (neg z'))
  go x = pure (D x)


-- copy a BDD over to a new tape and performs variable substitution
copy :: forall s s'. Reifies s' Cache => (Var -> BDD s') -> BDD s -> BDD s'
copy f n0 = evalState (go n0) HashMap.empty where
  go :: BDD s -> State (HashMap NodeId (BDD s')) (BDD s')
  go (ROBDD i v l r) 
    | i > 0 = gets (HashMap.lookup i) >>= \case
      Just z -> pure z
      Nothing -> do
        z <- ite (f v) <$> go l <*> go r  
        z <$ modify (HashMap.insert i z)
    | ni <- negate i = gets (HashMap.lookup ni) >>= \case
      Just z' -> pure (neg z')
      Nothing -> do
        z' <- ite (f v) <$> go l <*> go r  
        neg z' <$ modify (HashMap.insert ni z')
  go One  = pure One
  go Zero = pure Zero

-- work within one cache
copy' :: Reifies s Cache => (Var -> BDD s) -> BDD s -> BDD s
copy' = copy

-- relabel with a monotone function
copyMono :: forall s s'. Reifies s' Cache => (Var -> Var) -> BDD s -> BDD s'
copyMono f n0 = evalState (go n0) HashMap.empty where
  go :: BDD s -> State (HashMap NodeId (BDD s')) (BDD s')
  go (ROBDD i v l r) 
    | i > 0 = gets (HashMap.lookup i) >>= \case
      Just z -> pure z
      Nothing -> do
        z <- bdd (f v) <$> go l <*> go r  
        z <$ modify (HashMap.insert i z)
    | ni <- negate i = gets (HashMap.lookup ni) >>= \case
      Just z' -> pure (neg z')
      Nothing -> do
        z' <- bdd (f v) <$> go l <*> go r  
        neg z' <$ modify (HashMap.insert ni z')
  go One  = pure One
  go Zero = pure Zero

copyMono' :: Reifies s Cache => (Var -> Var) -> BDD s -> BDD s
copyMono' = copyMono
