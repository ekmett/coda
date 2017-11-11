module Meta3 where

data Neutral
  = S | S1 !Term | S2 !Term !Term
  | K | K1 !Term
  | I
  deriving Show

data Term
  = N !Neutral -- known neutral term
  | L Neutral -- thunk

instance Show Term where
  showsPrec d (N l) = showsPrec d l
  showsPrec d (L l) = showsPrec d l

infixl 9 `call`
call :: Neutral -> Term -> Neutral
call S (N K) = K1 (N I) -- SK=KI
call S x = S1 x
call (S1 x) y = S2 x y
call (S2 x y) z = eval x `call` z `call` app y z
call K x = K1 x
call (K1 x) _ = eval x
call I x = eval x

eval :: Term -> Neutral
eval (N t) = t
eval (L t) = t

app :: Term -> Term -> Term
app (N S) (N K) = N (K1 (N I)) -- SK=KI
app (N S) x = N (S1 x)  
app (N (S1 x)) y  = N (S2 x y)
app (N K) x = N (K1 x)
app (N I) x = x
app l r = L (eval l `call` r)

_S0, _K0, _I0 :: Term
_S0 = N S
_K0 = N K
_I0 = N I
