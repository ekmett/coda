{-# language DeriveFoldable #-}

---------------------------------------------------------------------------------
--- |
--- Copyright :  (c) Edward Kmett 2017
--- License   :  BSD2
--- Maintainer:  Edward Kmett <ekmett@gmail.com>
--- Stability :  experimental
--- Portability: portable
---
---------------------------------------------------------------------------------

module Coda.Data.Dyn
  ( Dyn
  , dcons
  ) where

import Data.Default
import Data.Monoid

-- | A functional version of Overmars and Van Leeuwen's 1983
-- dynamization schema.
--
-- If your queries are monoid homomorphisms from 'a' this takes
-- a static structure with an /O(n)/ ('<>') and gives you
-- dynamic one, with /O(log n)/ slower queries, but /O(log n)/ ('<>').
data Dyn a
  = D0
  | D1 !a
  | D2 !a !a a !(Dyn a)
  | D3 !a !a !a a !(Dyn a)
  deriving Foldable

instance Default (Dyn a) where
  def = D0

dcons :: Monoid a => a -> Dyn a -> Dyn a
dcons a D0 = D1 a
dcons a (D1 b) = D2 a b (a <> b) D0
dcons a (D2 b c bc ds) = D3 a b c bc ds
dcons a (D3 b _ _ cd es) = D2 a b (a <> b) (dcons cd es)
