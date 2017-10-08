{-# language CPP #-}
{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language BangPatterns #-}
{-# language MultiParamTypeClasses #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Okasaki real-time queue modified for /O(1)/ @rel@
--
---------------------------------------------------------------------------------

module Coda.Relative.Queue
  ( Queue((:<),Empty)
  , snocQ
  ) where

import Coda.Relative.Class
import Coda.Relative.Delta
import Coda.Relative.Foldable
import Control.Lens
import Data.Default
import Data.Function (on)
import Data.List (unfoldr)
import Data.Profunctor.Unsafe
import Data.Semigroup
import GHC.Exts as Exts
import Text.Read

-- @Q d f r s@ maintains @length s = length f - length r@
data Queue a = Q {-# unpack #-} !Delta [a] [a] [a]
#if __GLASGOW_HASKELL__ >= 802
{-# complete_patterns ((:<),Empty) #-}
#endif

instance Relative (Queue a) where
  rel 0 q = q
  rel d (Q d' f r s) = Q (d <> d') f r s

instance Default (Queue a) where
  def = Q 0 [] [] [] 

instance RelativeFoldable Queue where
  rnull (Q _ [] _ _) = True
  rnull _ = False

  rlength (Q _ _ rs ss) = length ss + 2 * length rs

  rfoldMap f d (Q d' fs rs _)
    | !d'' <- d <> d' = foldMap (f d'') fs `mappend` getDual (foldMap (Dual #. f d'') rs)

instance Relative a => IsList (Queue a) where
  type Item (Queue a) = a
  fromList = foldr cons def
  {-# inline fromList #-}
  toList = unfoldr uncons
  {-# inline toList #-}

instance AsEmpty (Queue a) where
  _Empty = prism (const $ Q 0 [] [] []) $ \case
    Q _ [] _ _ -> Right ()
    xs -> Left xs
  {-# inline _Empty #-}

instance (Relative a, Relative b) => Cons (Queue a) (Queue b) a b where
  _Cons = prism kons unkons where
    kons (a, Q d f r s) | a' <- rel (-d) a = Q d (a':f) r (a':s) 
    {-# inline conlike kons #-}
    unkons (Q _ [] _ _) = Left def
    unkons (Q d (x:f) r s) = Right (rel d x, exec d f r s)
  {-# inline _Cons #-}

snocQ :: Relative a => Queue a -> a -> Queue a
snocQ (Q d f r s) a = exec d f (rel (-d) a:r) s
{-# inline snocQ #-}

instance (Show a, Relative a) => Show (Queue a) where
  showsPrec d = showsPrec d . Exts.toList

instance (Read a, Relative a) => Read (Queue a) where
  readPrec = Exts.fromList <$> readPrec

instance (Eq a, Relative a) => Eq (Queue a) where
  (==) = (==) `on` Exts.toList
  {-# inline (==) #-}

instance (Ord a, Relative a) => Ord (Queue a) where
  compare = compare `on` Exts.toList
  {-# inline compare #-}

exec :: Delta -> [a] -> [a] -> [a] -> Queue a
exec d f r (_:s) = Q d f r s 
exec d f r [] = Q d f' [] f' where f' = rotate f r []
{-# inline exec #-}

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y:_) a = y:a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)
rotate _ _ _ = error "Coda.Relative.Queue.rotate: invariant broken"
