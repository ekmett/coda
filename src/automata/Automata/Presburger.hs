{-# language BangPatterns, RankNTypes #-}
module Automata.Presburger where

import Control.Lens
import Data.Bits hiding (complement)
import Data.Functor.Contravariant.Divisible
import GHC.Arr
import Numeric.Natural
import Prelude hiding (reverse)
import Utils.Containers.Internal.StrictPair

import Automata.Internal
import Automata.NFA
import Set.Lazy as Set

type Var = Int     -- "bad" is -1, as testBit x (-1) = 0
type Vec = Natural -- bit vector

var :: Eq a => a -> [a] -> Var
var a0 as0 = go 0 a0 as0 where
  go !_ _ [] = -1
  go i a (b:bs)
     | a == b = i
     | otherwise = go (i+1) a bs

-- common boolean sets
sb, sf, st :: Set Bool
sb = Set.fromDistinctAscList [False, True]
sf = Set.singleton False
st = Set.singleton True

given :: Bool -> a -> Set a
given True a = Set.singleton a
given False _ = Set.empty

-- i + j = k
add :: Var -> Var -> Var -> NFA Vec
add i j k = reverse $ NFA sb sf sf step where
  step xs b = case plus (testBit xs i) (testBit xs j) b of
    r :*: c -> given (testBit xs k == r) c
  plus a b c = case half a b of
    s1 :*: c1 -> case half s1 c of
      s2 :*: c2 -> s2 :*: (c1 || c2)
  half a b = xor a b :*: (a && b)

-- i + j + k = l
add3 :: Var -> Var -> Var -> Var -> NFA Vec
add3 i j k l = reverse $ NFA s3 s0 s0 step where
  step xs s = case plus3 (testBit xs i) (testBit xs j) (testBit xs k) s of
    r :*: c -> given (testBit xs l == r) c
  plus3 a b c d = r :*: div (d + fromEnum a + fromEnum b + fromEnum c - fromEnum r) 2 where r = a `xor` b `xor` c `xor` toEnum (mod d 2)
  s3 = Set.fromDistinctAscList [0,1,2]
  s0 = Set.singleton (0 :: Int)

eq :: Var -> Var -> NFA Vec
eq i j = NFA su su su $ \xs _ -> if testBit xs i == testBit xs j then su else empty
  where su = Set.singleton ()

data Cmp = Never | Eq | Lt | Le | Gt | Ge | Ne | Always deriving (Eq,Ord,Show,Read,Ix,Bounded,Enum)

cmp :: (forall a. Ord a => a -> a -> Bool) -> Var -> Var -> NFA Vec
cmp f i j = case toEnum (a + b + c) of
    Never -> complement conquer
    Eq -> eq i j
    Lt -> go st st empty
    Le -> go sb st empty
    Gt -> go st empty st
    Ge -> go sb empty st 
    Ne -> complement (eq i j)
    Always -> conquer
  where 
    a = if f True  False then 4 else 0
    b = if f False True  then 2 else 0
    c = if f False False then 1 else 0
    go :: Set Bool -> Set Bool -> Set Bool -> NFA Vec
    go fs fft ftf = NFA sb sf fs step where
      step _ True = st
      step xs False = case testBit xs i :*: testBit xs j of
        True :*: False -> ftf
        False :*: True -> fft
        _ -> sf

exists :: Var -> NFA Vec -> NFA Vec
exists n (NFA ss is fs d) = NFA ss (reachable d [0,bit n] is) fs $ \a s -> d a s `Set.union` d (setBit a n) s

forall :: Var -> NFA Vec -> NFA Vec
forall = over complemented . exists

data Bind = E | F

type Prefix = [Bind]

quantify :: Prefix -> NFA Vec -> NFA Vec
quantify = go 0 where
  go i (E:xs) b = exists i $ go (i+1) xs b
  go i (F:xs) b = forall i $ go (i+1) xs b
  go _ [] b = b

-- | is there a largest natural?
--
-- >>> check inf_exists
-- False
inf_exists :: NFA Vec
inf_exists = quantify [E,F] $ cmp (>=) 0 1

-- | is there a smallest natural?
--
-- >>> check zero_exists
-- True
zero_exists :: NFA Vec
zero_exists = quantify [E,F] $ cmp (<=) 0 1

-- | forall n. exists m. 2 * n == m
--
-- >>> check times2_exists
-- True
times2_exists :: NFA Vec
times2_exists = quantify [F,E] $ add 0 0 1

-- | forall n. exists m. 2 * m == n
--
-- >>> check div2_exists
-- False
div2_exists :: NFA Vec
div2_exists = quantify [F,E] $ add 1 1 0
