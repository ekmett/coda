{-# language DeriveDataTypeable #-}
{-# language PatternSynonyms #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}

module Coda.Algebra.Zero
  ( SemigroupWithZero(..)
  , WithZero(Zero,NonZero,WithZero,runWithZero)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Data.Data hiding (Prefix)
import Data.Semigroup
import Data.String
import GHC.Generics hiding (Prefix, prec)
import Prelude
import Text.Read

-- | @
-- zero <> a = zero = a <> zero
-- @
class Semigroup a => SemigroupWithZero a where
  zero :: a

instance SemigroupWithZero All where
  zero = All False

instance SemigroupWithZero Any where
  zero = Any True

instance SemigroupWithZero a => SemigroupWithZero (Dual a) where
  zero = Dual zero

instance Num a => SemigroupWithZero (Product a) where
  zero = Product 0

instance (Ord a, Bounded a) => SemigroupWithZero (Max a) where
  zero = Max maxBound

instance (Ord a, Bounded a) => SemigroupWithZero (Min a) where
  zero = Min minBound

-- | pushout
class (SemigroupWithZero a, Monoid a) => MonoidWithZero a
instance (SemigroupWithZero a, Monoid a) => MonoidWithZero a

-- adjoin a zero element to a semigroup
newtype WithZero a = WithZero { runWithZero :: Maybe a }
  deriving (Eq,Ord,Data,Generic,Generic1,Functor,Foldable,Traversable,Applicative,Alternative,Monad,MonadPlus,MonadZip)
{-# complete_patterns WithZero | (Zero, NonZero) #-}

pattern Zero :: WithZero a
pattern Zero = WithZero Nothing

pattern NonZero :: a -> WithZero a
pattern NonZero a = WithZero (Just a)

instance Show a => Show (WithZero a) where
  showsPrec d (NonZero a) = showParen (d > 10) $ showString "NonZero " . showsPrec 11 a
  showsPrec _ _Zero = showString "Zero"

instance Read a => Read (WithZero a) where
  readPrec = parens $ (prec 10 $ do Ident "Zero" <- lexP; return Zero)
                  +++ (prec 10 $ do Ident "NonZero" <- lexP; NonZero <$> step readPrec)
  readListPrec = readListPrecDefault

instance Semigroup a => Semigroup (WithZero a) where
  (<>) = liftA2 (<>)

instance Semigroup a => SemigroupWithZero (WithZero a) where
  zero = empty

instance Monoid a => Monoid (WithZero a) where
  mempty = empty
  mappend = liftA2 mappend

instance IsString a => IsString (WithZero a) where
  fromString = NonZero . fromString
