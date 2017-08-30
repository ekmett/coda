module Coda.Data.Dyn 
  ( Dyn
  ) where

-- | A functional version of Overmars and Van Leeuwen's 1983
-- dynamization schema. 
data Dyn a
  = D0
  | D1 !a
  | D2 !a !a a !(Dyn a)
  | D3 !a !a !a a !(Dyn a)
  deriving Foldable
  
mcons :: Monoid a => a -> Dyn a -> Dyn a
mcons a D0 = D1
mcons a (D1 b) = D2 a b (a <> b)
mcons a (D2 b c bc ds) = D3 a b c bc ds
mcons a (D3 b c d cd es) = D2 a b (a <> b) (cons cd es)
