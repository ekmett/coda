-- direct metacircular interpretation
module R where

import qualified Control.Monad.Fix as Fix

type Repr a = a

int :: Int -> Repr Int
int = id

bool :: Bool -> Repr Bool
bool = id

lam :: (Repr a -> Repr b) -> Repr (a -> b)
lam = id

app :: Repr (a -> b) -> Repr a -> Repr b
app = id

fix :: (Repr a -> Repr a) -> Repr a
fix = Fix.fix

add :: Repr Int -> Repr Int -> Repr Int
add = (+) 

mul :: Repr Int -> Repr Int -> Repr Int
mul = (*)

leq :: Repr Int -> Repr Int -> Repr Bool
leq = (<=) 

if_ :: Repr Bool -> Repr a -> Repr a -> Repr a
if_ x y z = if x then y else z
