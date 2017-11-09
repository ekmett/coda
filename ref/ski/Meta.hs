module Meta where

data Term = S | S1 Term | S2 Term Term | K | K1 Term | I deriving Show

infixl 9 `app`
app :: Term -> Term -> Term
app S x = S1 x
app (S1 x) y = S2 x y
app (S2 x y) z = x `app` z `app` (app y z)
app K x = K1 x
app (K1 x) _ = x
app I x = x

_S0, _K0, _I0 :: Term
_S0 = S
_K0 = K
_I0 = I
