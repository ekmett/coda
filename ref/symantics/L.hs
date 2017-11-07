-- length of a term
module L where

type Repr a = Int

int :: Int -> Repr Int
int _ = 1

bool :: Bool -> Repr Bool
bool _ = 1

lam :: (Repr a -> Repr b) -> Repr (a -> b)
lam f = f 0 + 1

app :: Repr (a -> b) -> Repr a -> Repr b
app f x = f + x + 1

fix :: (Repr a -> Repr a) -> Repr a
fix f = f 0 + 1

add :: Repr Int -> Repr Int -> Repr Int
add a b = a + b + 1

mul :: Repr Int -> Repr Int -> Repr Int
mul a b = a + b + 1

leq :: Repr Int -> Repr Int -> Repr Bool
leq a b = a + b + 1

if_ :: Repr Bool -> Repr a -> Repr a -> Repr a
if_ x y z = x + y + z + 1
