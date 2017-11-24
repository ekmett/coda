--------------------------------------------------------------------------------
-- | Efficient Boolean Pairs
--------------------------------------------------------------------------------
module Coda.Termination.Pair where

import Data.Bits
import Data.Semigroup
import GHC.Arr


-- boolean pairs
data BB = FF | FT | TF | TT deriving (Eq,Ord,Show,Read,Enum,Ix)

-- logical and
instance Semigroup BB where
  x <> y = toEnum (fromEnum x .&. fromEnum y)

instance Monoid BB where
  mempty = FF
  mappend = (<>)

bb1 :: BB -> Bool
bb1 x = testBit (fromEnum x) 2

bb2 :: BB -> Bool
bb2 x = testBit (fromEnum x) 1

bb :: Bool -> Bool -> BB
bb False False = FF
bb False True = FT
bb True False = TF
bb True True = TT

diagBB :: Bool -> BB
diagBB True  = TT
diagBB False = FF

ordBB :: Ordering -> BB
ordBB LT = TF
ordBB EQ = TT
ordBB GT = FT

pordBB :: Maybe Ordering -> BB
pordBB = maybe FF ordBB
