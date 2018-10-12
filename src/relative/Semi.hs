module Semi 
  ( Semi(..)
  ) where

import Delta
import Relative

--------------------------------------------------------------------------------
-- Semi-direct products
--------------------------------------------------------------------------------

data Semi a = Semi {-# unpack #-} !Delta a
  deriving (Eq,Ord,Show,Read)

instance RelativeSemigroup a => Semigroup (Semi a) where
  Semi a b <> Semi c d = Semi (a <> c) (b <> rel a d)

instance RelativeMonoid a => Monoid (Semi a) where
  mempty = Semi mempty mempty
  mappend = (<>)

instance Relative a => Relative (Semi a) where
  rel 0 xs = xs
  rel d (Semi d' a) = Semi (d+d') (rel d a)

instance RelativeSemigroup a => RelativeSemigroup (Semi a)
instance RelativeMonoid a => RelativeMonoid (Semi a)
instance RelativeOrder a => RelativeOrder (Semi a) 
instance StrictRelativeOrder a => StrictRelativeOrder (Semi a)
