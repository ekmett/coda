{-# language TypeFamilies #-}

-- partial evaluation
module P where

import qualified Control.Monad.Fix as Fix

import qualified R
import qualified Q

type family Static a :: * where
  Static Int = R.Repr Int
  Static Bool = R.Repr Bool
  Static (a -> b) = Repr a -> Repr b

data Repr a 
  = S (Static a) (Q.Repr a)
  | D (Q.Repr a)

abstr :: Repr a -> Q.Repr a
abstr (S _ q) = q
abstr (D q) = q

int :: Int -> Repr Int
int i = S (R.int i) (Q.int i)

bool :: Bool -> Repr Bool
bool b = S (R.bool b) (Q.bool b)

lam :: (Repr a -> Repr b) -> Repr (a -> b)
lam f = S f $ Q.lam $ \x -> abstr (f (D x))

app :: Repr (a -> b) -> Repr a -> Repr b
app f x = case f of
  S f' _ -> f' x
  _ -> D (Q.app (abstr f) (abstr x))

fix :: (Repr a -> Repr a) -> Repr a
fix = Fix.fix

add :: Repr Int -> Repr Int -> Repr Int
add (S 0 _) x = x
add x (S 0 _) = x
add (S m _) (S n _) = int (R.add m n)
add x y = D (Q.add (abstr x) (abstr y))

mul :: Repr Int -> Repr Int -> Repr Int
mul (S 0 _) _ = int 0
mul (S 1 _) x = x
mul _ (S 0 _) = int 0
mul x (S 1 _) = x
mul (S m _) (S n _) = int (R.mul m n)
mul x y = D $ Q.mul (abstr x) (abstr y)

leq :: Repr Int -> Repr Int -> Repr Bool
leq (S a _) (S b _) = bool (R.leq a b)
leq x y = D $ Q.leq (abstr x) (abstr y)

if_ :: Repr Bool -> Repr a -> Repr a -> Repr a
if_ (S False _) _ z = z
if_ (S True _) y _ = y
if_ x y z = D $ Q.if_ (abstr x) (abstr y) (abstr z)
