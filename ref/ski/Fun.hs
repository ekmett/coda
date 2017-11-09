{-# language TypeFamilies #-}
module Fun where

import Term

class Fun t where
  term :: Term -> [Term] -> t

instance Fun Term where
  term t xs = foldr (flip app) t xs

instance (tm ~ Term, Fun t) => Fun (tm -> t) where
  term x xs y = term x (y:xs)

_S, _K, _I :: Fun t => t
_S = term _S0 []
_K = term _K0 []
_I = term _I0 []
