{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language DeriveTraversable #-}
{-# language BangPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language TemplateHaskell #-}

module Coda.Sequence 
  ( Seq(Empty,(:<),(:>))
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Foldable as Foldable (toList)
import Data.Function (on)
import Data.Functor.Day.Curried
import Data.Functor.Yoneda
import qualified Data.List as List
import Data.Semigroup
import Data.Traversable (foldMapDefault, fmapDefault)
import GHC.Exts (IsList(..))
import Text.Read

-- a pair of weighted elements
data P a
  = P a !Int a
  deriving Foldable

-- a 'digit', one or two weighted elements
data D a
  = D1 a
  | D2 a !Int a
  deriving Foldable

-- a deque containing 2 or more elements
data Q a 
  = Q2 a !Int a !Int
  | Q3 a !Int a !Int a !Int
  | Q4 a !Int a !Int a !Int a !Int
  | Q5 a !Int a !Int a !Int a !Int a !Int
  | QN !(D a) !Int (Q (P a)) !Int !(D a) !Int
  deriving Foldable

-- an O(1) catenable deque with O(log n) weighted indexing
data C a
  = C0
  | C1 a
  | CS !(Q a)
  | CD !(Q a) (C (Q a)) !Int !(Q a)
  deriving Foldable

class Weighted a where
  weight :: a -> Int

instance Weighted (Q a) where
  weight (Q2 _ _ _ w) = w
  weight (Q3 _ _ _ _ _ w) = w
  weight (Q4 _ _ _ _ _ _ _ w) = w
  weight (Q5 _ _ _ _ _ _ _ _ _ w) = w
  weight (QN _ _ _ _ _ w) = w

instance Weighted a => Weighted (C a) where
  weight C0 = 0
  weight (C1 a) = weight a
  weight (CS q) = weight q
  weight (CD l _ wm r) = weight l + wm + weight r

-- cons a weighted element into the Q
consQ :: Int -> a -> Q a -> Q a
consQ wa a (Q2 b wb c wbc) = Q3 a wa b (wa+wb) c (wa+wbc)
consQ wa a (Q3 b wb c wbc d wbcd) = Q4 a wa b (wa+wb) c (wa+wbc) d (wa+wbcd)
consQ wa a (Q4 b wb c wbc d wbcd e wbcde)  = Q5 a wa b (wa+wb) c (wa+wbc) d (wa+wbcd) e (wa+wbcde)
consQ wa a (Q5 b wb c wbc d wbcd e wbcde f wbcdef)
  | wd <- wbcd-wbc, wabcde <- wa+wbcde, wabcdef <- wa+wbcdef
  = QN (D1 a) wa (Q2 (P b wb c) wbc (P d wd e) wbcde) wabcde (D1 f) wabcdef
consQ wa a (QN (D1 b) wb m wlm r wlmr) | wab <- wa + wb = QN (D2 a wa b) wab m (wa+wlm) r (wa+wlmr)
consQ wa a (QN (D2 b wb c) wbc m wlm r wlmr) = QN (D1 a) wa (consQ wbc (P b wb c) m) (wa+wlm) r (wa+wlmr)
    
-- snoc a weighted element onto the Q
snocQ :: Q a -> Int -> a -> Q a
snocQ (Q2 a wa b wab) wc c = Q3 a wa b wab c (wab+wc)
snocQ (Q3 a wa b wab c wabc) wd d = Q4 a wa b wab c wabc d (wabc+wd)
snocQ (Q4 a wa b wab c wabc d wabcd) we e = Q5 a wa b wab c wabc d wabcd e (wabcd+we)
snocQ (Q5 a wa b wab c wabc d wabcd e wabcde) wf f
  | !wb <- wab-wa, !wd <- wabcd-wabc, !wabcdef <- wabcde + wf, !wbcde <- wabcde-wa
  = QN (D1 a) wa (Q2 (P b wb c) wabc (P d wd e) wbcde) wabcde (D1 f) wabcdef
snocQ (QN l wl m wlm (D1 a) wlma) wb b | wa <- wlma - wlm = QN l wl m wlm (D2 a wa b) (wlma+wb)
snocQ (QN l wl m wlm (D2 a wa b) wlmab) wc c | wab <- wlmab-wlm = QN l wl (snocQ m wab (P a wa b)) (wlm+wab) (D1 c) (wlmab+wc)

unconsQ :: Q a -> (a, Int, Either a (Q a))
unconsQ (Q2 a wa b _)                           = (a, wa, Left b)
unconsQ (Q3 a wa b wab c wabc)                  = (a, wa, Right $! Q2 b (wab-wa) c (wabc-wa))
unconsQ (Q4 a wa b wab c wabc d wabcd)          = (a, wa, Right $! Q3 b (wab-wa) c (wabc-wa) d (wabcd-wa))
unconsQ (Q5 a wa b wab c wabc d wabcd e wabcde) = (a, wa, Right $! Q4 b (wab-wa) c (wabc-wa) d (wabcd-wa) e (wabcde-wa))
unconsQ (QN (D2 a wa b) wl m wlm r wlmr)        = (a, wa, Right $! QN (D1 b) (wl-wa) m (wlm-wa) r (wlmr-wa))
unconsQ (QN (D1 a) wa m wlm r wlmr)             = (a, wa, Right q') where
  q' = case unconsQ m of
    (P b wb c, wbc, Right q) -> QN (D2 b wb c) wbc q (wlm-wa) r (wlmr-wa)
    (P b wb c, wbc, Left (P d wd e)) -> case r of
      D1 f | !wm <- wlm-wa, !wde <- wm-wbc -> Q5 b wb c wbc d (wbc+wd) e (wbc+wde) f (wlmr-wa)
      D2 f wf g
        | !wm <- wlm-wa, !wde <- wm-wbc, wc <- wbc-wb, wcd <- wc+wd, we <- wde-wd, wef <- we+wf, wcdef <- wcd+wef
        -> QN (D1 b) wb (Q2 (P c wc d) wcd (P e we f) wcdef) (wb+wcdef) (D1 g) (wlmr-wa)

unsnocQ :: Q a -> (Either a (Q a), a, Int)
unsnocQ (Q2 a wa b wab)                         | !wb <- wab - wa       = (Left a, b, wb)
unsnocQ (Q3 a wa b wab c wabc)                  | !wc <- wabc - wab     = (Right $! Q2 a wa b wab, c, wc)
unsnocQ (Q4 a wa b wab c wabc d wabcd)          | !wd <- wabcd - wabc   = (Right $! Q3 a wa b wab c wabc, d, wd)
unsnocQ (Q5 a wa b wab c wabc d wabcd e wabcde) | !we <- wabcde - wabcd = (Right $! Q4 a wa b wab c wabc d wabcd, e, we)
unsnocQ (QN l wl m wlm (D2 a wa b) wlmr)
  | !wr <- wlmr-wlm, !wb <- wr-wa = (Right $! QN l wl m wlm (D1 a) (wlmr-wb), b, wb)
unsnocQ (QN l wl m wlm (D1 a) wlmr) = (Right q', a, wa) where
  !wa = wlmr - wlm
  q' = case unsnocQ m of
    (Right m', P b wb c, wbc) -> QN l wl m' (wlm-wbc) (D2 b wb c) wlm
    (Left (P b wb c), P d wd e, wde) -> case l of
      D1 f | wm <- wlm-wl, wbc <- wm-wde -> Q5 b wb c wbc d (wbc+wd) e (wbc+wde) f (wlmr-wa)
      D2 f wf g
        | wm <- wlm-wl, wbc <- wm-wde, wc <- wbc-wb, wcd <- wc+wd, we <- wde-wd, wef <- we+wf, wcdef <- wcd+wef
        -> QN (D1 b) wb (Q2 (P c wc d) wcd (P e we f) wcdef) (wb+wcdef) (D1 g) (wlmr-wa)
      
instance Weighted b => Cons (C a) (C b) a b where
  _Cons = prism kons unkons where
    kons :: Weighted b => (b, C b) -> C b
    kons (a, C0) = C1 a
    kons (a, C1 b) | wa <- weight a = CS (Q2 a wa b (wa+weight b))
    kons (a, CS q) = CS (consQ (weight a) a q)
    kons (a, CD l m wm r) = CD (consQ (weight a) a l) m wm r

    unkons :: C a -> Either (C b) (a, C a)
    unkons C0 = Left C0
    unkons (C1 a) = Right (a, C0)
    unkons (CS q) = Right $ case unconsQ q of
      (a, _, Left b) -> (a, C1 b) 
      (a, _, Right q') -> (a, CS q')
    unkons (CD l m wm r) = Right $ case unconsQ l of
      (a, wa, ebl') -> (a,q') where
        q' = case ebl' of
          Right l' -> CD l' m wm r
          Left b | wb <- weight l - wa -> case unkons m of
            Right (l', m') -> CD (consQ wb b l') m' (wm-weight l') r
            Left{} -> CS (consQ wb b r)

instance Weighted b => Snoc (C a) (C b) a b where
  _Snoc = prism snok unsnok where
    snok :: Weighted b => (C b, b) -> C b
    snok (C0, a) = C1 a
    snok (C1 a, b) | wa <- weight a = CS (Q2 a wa b (wa+weight b))
    snok (CS q, a) = CS (snocQ q (weight a) a)
    snok (CD l m wm r, a) = CD l m wm (snocQ r (weight a) a)

    unsnok :: C a -> Either (C b) (C a, a)
    unsnok C0 = Left C0
    unsnok (C1 a) = Right (C0, a)
    unsnok (CS q) = Right $ case unsnocQ q of
      (Left a, b, _) -> (C1 a, b) 
      (Right q', a, _) -> (CS q', a)
    unsnok (CD l m wm r) = Right $ case unsnocQ r of
      (ebr', a, wa) -> (q',a) where
        q' = case ebr' of
          Right r' -> CD l m wm r'
          Left b | wb <- weight r - wa -> case unsnok m of
            Right (m', r') -> CD l m' (wm-weight r') (snocQ r' wb b)
            Left{} -> CS (snocQ l wb b)

instance Weighted a => IsList (C a) where
  type Item (C a) = a
  fromList = foldr cons mempty
  toList = Foldable.toList

instance Eq a => Eq (C a) where
  (==) = (==) `on` Foldable.toList

instance Ord a => Ord (C a) where
  compare = compare `on` Foldable.toList

instance Show a => Show (C a) where
  showsPrec d = showsPrec d . Foldable.toList

instance (Weighted a, Read a) => Read (C a) where
  readPrec = fromList <$> readPrec

instance Weighted a => Semigroup (C a) where
  C0 <> a = a
  a <> C0 = a
  C1 a <> d = cons a d
  a <> C1 b = snoc a b
  CS q1 <> CS q2 = CD q1 C0 0 q2
  CS q <> CD l m wm r = CD q (cons l m) (weight l + wm) r
  CD l m wm r <> CS q = CD l (snoc m r) (wm + weight r) q
  CD l m wm r <> CD l' m' wm' r' = CD l (snoc m r <> cons l' m') (wm + weight r + weight l' + wm') r'

instance Weighted a => Monoid (C a) where
  mempty = C0
  mappend = (<>)

-- | Everything can be weighted with weight 1.
newtype One a = One a
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

makePrisms ''One

instance Weighted (One a) where
  weight _ = 1

-- | O(1) Catenable deques with O(log n) time indexing
--
-- Most of the API is provided by standard combinators:
--
-- @
-- 'null', 'length', 'empty', 'pure', '(<|>)' or '(<>)', etc.
-- @
--
-- Many other combinators and traversals come from @lens@:
--
-- @
-- 'cons', 'uncons', 'snoc', 'unsnoc', '_head', '_tail', 'ix', 'itraversed'
-- @
--
-- as well as the patterns for '(:<)', '(:>)' and 'Empty'
--
-- The @OverloadedLists@ extension can be used as well.

newtype Seq a = Seq (C (One a))
  deriving (Eq,Ord,Semigroup,Monoid)

#if __GLASGOW_HASKELL__ >= 802
{-# complete_patterns ((:<), Empty) | (:>),Empty) #-}
#endif

makePrisms ''Seq

instance Show a => Show (Seq a) where
  showsPrec d = showsPrec d . Foldable.toList

instance Read a => Read (Seq a) where
  readPrec = fromList <$> readPrec

instance AsEmpty (Seq a) where
  _Empty = prism (\() -> Seq C0) $ \case
    Seq C0 -> Right ()
    q  -> Left q

instance Applicative Seq where
  pure a = Seq (C1 (One a))
  {-# inline pure #-}
  mf <*> ma = foldMap (\a -> ($ a) <$> mf) ma
  {-# inline (<*>) #-}
  (*>) = (>>) 
  {-# inline (*>) #-}

instance Monad Seq where
  (>>=) = flip foldMap 
  {-# inline (>>=) #-}

instance Alternative Seq where
  empty = Seq C0
  {-# inline empty #-}
  (<|>) = (<>)
  {-# inline (<|>) #-}
  
instance MonadPlus Seq where
  mzero = empty
  mplus = (<|>)

instance Weighted (Seq a) where
  weight (Seq l) = weight l

instance Cons (Seq a) (Seq b) a b where
  _Cons = _Seq._Cons.bimapping _One (from _Seq)

instance Snoc (Seq a) (Seq b) a b where
  _Snoc = _Seq._Snoc.bimapping (from _Seq) _One

instance IsList (Seq a) where
  type Item (Seq a) = a
  fromList = foldr cons mempty
  toList = Foldable.toList

instance Functor Seq where
  fmap = fmapDefault

instance Foldable Seq where
  foldMap = foldMapDefault

  length = weight

  null (Seq C0) = True
  null _ = False

instance Traversable Seq where
  traverse f0 xs = confusing (_Seq.traverseC._One) f0 xs where
    -- not traversable in their own right as these combinators are only for weight-preserving transformations
    traverseC :: Applicative f => (a -> f b) -> C a -> f (C b)
    traverseC _ C0 = pure C0
    traverseC f (C1 a) = C1 <$> f a
    traverseC f (CS q) = CS <$> traverseQ f q
    traverseC f (CD l m wm r) = (\l' m' r' -> CD l' m' wm r') <$> traverseQ f l <*> traverseC (traverseQ f) m <*> traverseQ f r

    traverseQ :: Applicative f => (a -> f b) -> Q a -> f (Q b)
    traverseQ f (Q2 a wa b wab) = (\a' b' -> Q2 a' wa b' wab) <$> f a <*> f b
    traverseQ f (Q3 a wa b wab c wabc) = (\a' b' c' -> Q3 a' wa b' wab c' wabc) <$> f a <*> f b <*> f c
    traverseQ f (Q4 a wa b wab c wabc d wabcd) = (\a' b' c' d' -> Q4 a' wa b' wab c' wabc d' wabcd) <$> f a <*> f b <*> f c <*> f d
    traverseQ f (Q5 a wa b wab c wabc d wabcd e wabcde) = (\a' b' c' d' e'-> Q5 a' wa b' wab c' wabc d' wabcd e' wabcde) <$> f a <*> f b <*> f c <*> f d <*> f e
    traverseQ f (QN l wl m wlm r wlmr) = (\l' m' r' -> QN l' wl m' wlm r' wlmr) <$> traverseD f l <*> traverseQ (traverseP f) m <*> traverseD f r

    traverseD :: Applicative f => (a -> f b) -> D a -> f (D b)
    traverseD f (D1 a) = D1 <$> f a
    traverseD f (D2 a wa b) = (\a' b' -> D2 a' wa b') <$> f a <*> f b

    traverseP :: Applicative f => (a -> f b) -> P a -> f (P b)
    traverseP f (P a wa b) = (\a' b' -> P a' wa b') <$> f a <*> f b
  {-# inline traverse #-}

instance FoldableWithIndex Int Seq
instance FunctorWithIndex Int Seq
instance TraversableWithIndex Int Seq where
  itraverse f0 (Seq xs) = lowerYoneda (lowerCurried (Seq <$> atraverseC (atraverseOne (\i -> liftCY . f0 i)) 0 xs)) where
    liftCY :: Applicative f => f a -> Curried (Yoneda f) (Yoneda f) a
    liftCY fa = Curried $ \(Yoneda k) -> Yoneda (\ab_r -> k (ab_r .) <*> fa)
    {-# inline liftCY #-}

    atraverseOne :: Applicative f => (Int -> a -> f b) -> Int -> One a -> f (One b)
    atraverseOne f !acc (One a) = One <$> f acc a
    {-# inline atraverseOne #-}

    atraverseC :: Applicative f => (Int -> a -> f b) -> Int -> C a -> f (C b)
    atraverseC _ !_ C0 = pure C0
    atraverseC f acc (C1 a) = C1 <$> f acc a
    atraverseC f acc (CS q) = CS <$> atraverseQ f acc q
    atraverseC f acc (CD l m wm r) = (\l' m' r' -> CD l' m' wm r') <$> atraverseQ f acc l <*> (atraverseC (atraverseQ f) $! acc + wl) m <*> (atraverseQ f $! acc + wlm) r
      where wl = weight l; wlm = wl + wm

    atraverseQ :: Applicative f => (Int -> a -> f b) -> Int -> Q a -> f (Q b)
    atraverseQ f !acc (Q2 a wa b wab)
      = (\a' b' -> Q2 a' wa b' wab) <$> f acc a <*> f (acc + wa) b
    atraverseQ f acc (Q3 a wa b wab c wabc)
      = (\a' b' c' -> Q3 a' wa b' wab c' wabc) <$> f acc a <*> (f $! acc + wa) b <*> (f $! acc + wab) c
    atraverseQ f acc (Q4 a wa b wab c wabc d wabcd)
      = (\a' b' c' d' -> Q4 a' wa b' wab c' wabc d' wabcd) <$> f acc a <*> (f $! acc + wa) b <*> (f $! acc + wab) c <*> (f $! acc + wabc) d
    atraverseQ f acc (Q5 a wa b wab c wabc d wabcd e wabcde)
      = (\a' b' c' d' e'-> Q5 a' wa b' wab c' wabc d' wabcd e' wabcde) <$> f acc a <*> (f $! acc + wa) b <*> (f $! acc+wab) c <*> (f $! acc+wabc) d <*> (f $! acc+wabcd) e
    atraverseQ f acc (QN l wl m wlm r wlmr)
      = (\l' m' r' -> QN l' wl m' wlm r' wlmr) <$> atraverseD f acc l <*> (atraverseQ (atraverseP f) $! acc+wl) m <*> (atraverseD f $! acc+wlm) r

    atraverseD :: Applicative f => (Int -> a -> f b) -> Int -> D a -> f (D b)
    atraverseD f !acc (D1 a) = D1 <$> f acc a
    atraverseD f acc (D2 a wa b) = (\a' b' -> D2 a' wa b') <$> f acc a <*> (f $! acc+wa) b
    {-# inline atraverseD #-}

    atraverseP :: Applicative f => (Int -> a -> f b) -> Int -> P a -> f (P b)
    atraverseP f !acc (P a wa b) = (\a' b' -> P a' wa b') <$> f acc a <*> (f $! acc+wa) b
  {-# inline itraverse #-}


type instance IxValue (Seq a) = a
type instance Index (Seq a) = Int

instance Ixed (Seq a) where
  ix i0 f0 (Seq q0)
    | 0 <= i0, i0 < weight q0 = lowerYoneda (Seq <$> aixC (\_ -> liftYoneda . _One f0) i0 q0)
    | otherwise = pure (Seq q0)
    where
      aixP :: (Int -> a -> Yoneda f a) -> Int -> P a -> Yoneda f (P a)
      aixP f i (P a wa b)
        | i < wa    = f i a           <&> \a' -> P a' wa b
        | otherwise = (f $! i - wa) b <&> \b' -> P a wa b'
      {-# inline aixP #-}
   
      aixD :: (Int -> a -> Yoneda f a) -> Int -> D a -> Yoneda f (D a)
      aixD f i (D1 a) = D1 <$> f i a 
      aixD f i (D2 a wa b)
        | i < wa = f i a <&> \a' -> D2 a' wa b
        | otherwise = (f $! i - wa) b <&> \b' -> D2 a wa b'
      {-# inline aixD #-}
   
      aixQ :: (Int -> a -> Yoneda f a) -> Int -> Q a -> Yoneda f (Q a)
      aixQ f i (Q2 a wa b wab)
        | i < wa    = f i           a <&> \a' -> Q2 a' wa b wab
        | otherwise = (f $! i - wa) b <&> \b' -> Q2 a wa b' wab
      aixQ f i (Q3 a wa b wab c wabc)
        | i < wa    = f i            a <&> \a' -> Q3 a' wa b wab c wabc
        | i < wab   = (f $! i - wa)  b <&> \b' -> Q3 a wa b' wab c wabc
        | otherwise = (f $! i - wab) c <&> \c' -> Q3 a wa b wab c' wabc
      aixQ f i (Q4 a wa b wab c wabc d wabcd)
        | i < wa    = f i             a <&> \a' -> Q4 a' wa b wab c wabc d wabcd
        | i < wab   = (f $! i - wa)   b <&> \b' -> Q4 a wa b' wab c wabc d wabcd
        | i < wabc  = (f $! i - wab)  c <&> \c' -> Q4 a wa b wab c' wabc d wabcd
        | otherwise = (f $! i - wabc) d <&> \d' -> Q4 a wa b wab c wabc d' wabcd
      aixQ f i (Q5 a wa b wab c wabc d wabcd e wabcde)
        | i < wa    = f i              a <&> \a' -> Q5 a' wa b wab c wabc d wabcd e wabcde
        | i < wab   = (f $! i - wa)    b <&> \b' -> Q5 a wa b' wab c wabc d wabcd e wabcde
        | i < wabc  = (f $! i - wab)   c <&> \c' -> Q5 a wa b wab c' wabc d wabcd e wabcde
        | i < wabcd = (f $! i - wabc)  d <&> \d' -> Q5 a wa b wab c wabc d' wabcd e wabcde
        | otherwise = (f $! i - wabcd) e <&> \e' -> Q5 a wa b wab c wabc d wabcd e' wabcde
      aixQ f i (QN l wl m wlm r wlmr)
        | i < wl    = aixD f i l                  <&> \l' -> QN l' wl m wlm r wlmr
        | i < wlm   = (aixQ (aixP f) $! i - wl) m <&> \m' -> QN l wl m' wlm r wlmr
        | otherwise = (aixD f $! i-wlm) r         <&> \r' -> QN l wl m wlm r' wlmr
   
      aixC :: (Int -> a -> Yoneda f a) -> Int -> C a -> Yoneda f (C a)
      aixC _ _ C0 = error "ix (Seq a): invariant broken"
      aixC f i (C1 a) = C1 <$> f i a
      aixC f i (CS q) = CS <$> aixQ f i q
      aixC f i (CD l m wm r)
        | i < wl = aixQ f i l <&> \l' -> CD l' m wm r
        | i < wlm = (aixC (aixQ f) $! i - wl) m <&> \m' -> CD l m' wm r
        | otherwise = (aixQ f $! i - wlm) r <&> CD l m wm
        where wl = weight l; wlm = wl + wm
  {-# inline ix #-}
