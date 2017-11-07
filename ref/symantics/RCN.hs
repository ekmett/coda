{-# language RankNTypes, TypeFamilies #-}
-- call-by-need interpreter
module RCN where

import qualified R

type family Static a :: * where
  Static Int = R.Repr Int
  Static Bool = R.Repr Bool
  Static (a -> b) = Repr a -> Repr b

newtype Repr a = Repr { runRepr :: forall r. (Static a -> r) -> r }

int :: Int -> Repr Int
int x = Repr $ \k -> k x
bool :: Bool -> Repr Bool
bool x = Repr $ \k -> k x

lam :: (Repr a -> Repr b) -> Repr (a -> b)
lam f = Repr $ \k -> k f

app :: Repr (a -> b) -> Repr a -> Repr b
app a b = Repr $ \k -> runRepr a $ \f -> runRepr (f b) k

fix :: (Repr a -> Repr a) -> Repr a
fix = undefined --TODO
-- fix f0 = let fx f n = app (f (lam (fx f))) n in lam (fx f0)

add :: Repr Int -> Repr Int -> Repr Int
add a b = Repr $ \k -> runRepr a $ \v1 -> runRepr b $ \v2 -> k (v1 + v2)

mul :: Repr Int -> Repr Int -> Repr Int
mul a b = Repr $ \k -> runRepr a $ \v1 -> runRepr b $ \v2 -> k (v1 * v2)

leq :: Repr Int -> Repr Int -> Repr Bool
leq a b = Repr $ \k -> runRepr a $ \v1 -> runRepr b $ \v2 -> k (v1 <= v2)

if_ :: Repr Bool -> Repr a -> Repr a -> Repr a
if_ eb et ee = Repr $ \k -> runRepr eb $ \v -> if v then runRepr et k else runRepr ee k
