{-# language TemplateHaskell #-}
-- staged compilation via template-haskell
module Q where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Repr a = Q Exp

int :: Int -> Repr Int
int = lift

bool :: Bool -> Repr Bool
bool = lift

lam :: (Repr a -> Repr b) -> Repr (a -> b)
lam f = do
  n <- newName  "x"
  lamE [varP n] (f (varE n))

app :: Repr (a -> b) -> Repr a -> Repr b
app = appE

fix :: (Repr a -> Repr a) -> Repr a
fix f = do
  n <- newName "x"
  letE [valD (varP n) (normalB (f (varE n))) []] (varE n)

add :: Repr Int -> Repr Int -> Repr Int
add x y = varE '(+) `appE` x `appE` y

mul :: Repr Int -> Repr Int -> Repr Int
mul x y = varE '(*) `appE` x `appE` y

leq :: Repr Int -> Repr Int -> Repr Bool
leq x y = varE '(<=) `appE` x `appE` y

if_ :: Repr Bool -> Repr a -> Repr a -> Repr a
if_ = condE
