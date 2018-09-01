{-# language CPP #-}
{-# language OverloadedLists #-}
{-# language LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Layout where

import Control.Lens
import Data.Default

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif

import Relative.Class
import Relative.Delta
import Relative.Cat as Cat
import Rev
import Syntax.Prefix

import Dyck
-- import Token

data LayoutMismatch = LayoutMismatch !Delta !Prefix !Prefix

instance Relative LayoutMismatch where
  rel d (LayoutMismatch d' p q) = LayoutMismatch (d <> d') p q

data Run = Run {-# unpack #-} !Prefix !(Cat Dyck) {-# unpack #-} !Dyck !(Cat LayoutMismatch)

instance Relative Run where
  rel d (Run p ds ts es) = Run p (rel d ds) (rel d ts) (rel d es)

runDyck :: Run -> Dyck
runDyck (Run _ _ ts _) = ts

runsDyck :: Cat Run -> Dyck
runsDyck Empty = Empty
runsDyck (x :< xs) = runDyck x <> runsDyck xs

instance HasPrefix Run where
  prefix (Run p _ _ _) = p

data Layout
  = E {-# unpack #-} !Delta
  | S {-# unpack #-} !Delta {-# unpack #-} !Run
  | V {-# unpack #-} !Delta !(Cat Run) {-# unpack #-} !Run !(Rev Cat Run)

instance HasDelta Layout where
  delta (E d) = d
  delta (S d _) = d
  delta (V d _ _ _) = d

instance AsEmpty Layout where
  _Empty = prism (const $ E 0) $ \case
    E 0 -> Right ()
    x   -> Left x

dyckLayout :: Delta -> Prefix -> Dyck -> Layout
dyckLayout d _ Empty = E d
dyckLayout d p t = S d $ Run p [t] t []

boring :: Dyck -> Bool
boring = views dyckLayoutMode (def ==)

instance Semigroup Layout where
  E 0 <> xs = xs
  xs <> E 0 = xs
  E d <> E d' = E (d <> d')
  E d <> S d' (Run p ds ts es) = S (d <> d') $ Run p (rel d ds) (rel d ts) (rel d es)
  E d <> V d' l m r = V (d <> d') (rel d l) m (rel d r)
  S d (Run p ds ts es) <> E d' = S (d <> d') $ Run p ds ts es
  S d lr@(Run p ds ts es) <> S d' rr@(Run p' ds' ts' es') = case joinAndCompare p p' of
    Left p'' -> S (d <> d') $ Run p'' (ds <> rel d ds') (ts <> rel d ts') (snocCat es (LayoutMismatch d p p') <> rel d es') -- no common prefix
    Right LT -- indent
      | boring ts -> S (d <> d') $ Run p (ds <> rel d ds') (ts <> rel d ts') Empty
      | otherwise -> V (d <> d') Empty lr $ Rev $ Cat.singleton (rel d rr)
    Right EQ -> S (d <> d') $ Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es')
    Right GT -> V (d <> d') (Cat.singleton lr) (rel d rr) Empty
  --S d lr@(Run p ds ts es) <> V d' l m r = case joinAndCompare p (prefix m) of
  --   Left p'' | has _Empty r -> undefined -- S $ Run (d <> d') p'' 
     -- ... -- TODO: resume here
       
instance Monoid Layout where
  mempty = E 0
  mappend = (<>)
