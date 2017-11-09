module Direct where

data Neutral = S0 | S1 !Term | S2 !Term !Term | K0 | K1 !Term | I0 deriving Show
data Term = N Neutral | A Neutral !Term !Term

instance Show Term where
  showsPrec d (N l) = showsPrec d l
  showsPrec d (A _ l r) = showParen (d > 10) $ showString "A " . showsPrec 11 l . showChar ' ' . showsPrec 11 r

infixl 9 `call`
call :: Neutral -> Term -> Neutral
call S0 (N K0) = K1 (N I0) -- SK=KI
call S0 x = S1 x
call (S1 x) y = S2 x y
call (S2 x y) z = eval x `call` z `call` app y z
call K0 x = K1 x
call (K1 x) _ = eval x
call I0 x = eval x

eval :: Term -> Neutral
eval (N t) = t
eval (A t _ _) = t

-- application with opportunistic optimization
app :: Term -> Term -> Term
app (N S0) (N K0) = N (K1 (N I0)) -- SK=KI
app (N S0) x      = N (S1 x)  
app (N (S1 x)) y  = N (S2 x y)
app (N K0) x      = N (K1 x)
app (N I0) x      = x
app l r = A (eval l `call` r) l r

_S0, _K0, _I0 :: Term
_S0 = N S0
_K0 = N K0
_I0 = N I0
