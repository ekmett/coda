{-# language CPP #-}
{-# language OverloadedLists #-}
{-# language LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Layout where

import Control.Lens

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif

import Relative.Class
import Relative.Delta
import Relative.Cat as Cat
import Rev
import Syntax.Prefix

import Dyck

data LayoutMismatch = LayoutMismatch !Delta !Prefix !Prefix
  deriving (Eq, Show) -- this is for debugging the Layout Monoid

instance Relative LayoutMismatch where
  rel d (LayoutMismatch d' p q) = LayoutMismatch (d <> d') p q

-- The first Prefix is the lowest indent that covers the whole run
-- The second Prefix is the indent that covers the last line we've seend
data Run = Run {-# unpack #-} !Prefix !(Cat Dyck) {-# unpack #-} !Dyck !(Cat LayoutMismatch) !Prefix
  deriving (Eq, Show) -- this is for debugging the Layout Monoid

instance Relative Run where
  rel d (Run p ds ts es pr) = Run p (rel d ds) (rel d ts) (rel d es) pr

runDyck :: Run -> Dyck
runDyck (Run _ _ ts _ _) = ts

runsDyck :: Cat Run -> Dyck
runsDyck Empty = Empty
runsDyck (x :< xs) = runDyck x <> runsDyck xs

instance HasPrefix Run where
  prefix (Run p _ _ _ _) = p

runDycks :: Run -> Cat Dyck
runDycks (Run _ ds _ _ _) = ds

runsDycks :: Cat Run -> Cat Dyck
runsDycks Empty = Empty
runsDycks (x :< xs) = runDycks x <> runsDycks xs

runMismatch :: Run -> Cat LayoutMismatch
runMismatch (Run _ _ _ es _) = es

runsMismatch :: Cat Run -> Cat LayoutMismatch
runsMismatch Empty = Empty
runsMismatch (x :< xs) = runMismatch x <> runsMismatch xs

data Layout
  = E {-# unpack #-} !Delta
  | S {-# unpack #-} !Delta {-# unpack #-} !Run
  -- It might be worth adding a Cat Run after the Run for the middle of the V,
  -- to track things which have the same indent as the middle of the V.
  -- We can snoc to Cat Run, and if we need to reverse it to shuffle things it should
  -- be shorter than the rightmost part of the V in most cases.
  | V {-# unpack #-} !Delta !(Cat Run) {-# unpack #-} !Run !(Rev Cat Run)
  deriving (Eq, Show) -- this is for debugging the Layout Monoid

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
dyckLayout d p t = S d $ Run p [t] t [] p

gatherMismatches :: Layout -> Cat LayoutMismatch
gatherMismatches (E _) = Empty
gatherMismatches (S _ r) = runMismatch r
gatherMismatches (V _ l m (Rev r)) = runsMismatch l <> runMismatch m <> runsMismatch r

gatherMismatchPrefixes :: Cat LayoutMismatch -> [Prefix]
gatherMismatchPrefixes Empty = []
gatherMismatchPrefixes (LayoutMismatch _ p1 p2 :< lms) = p1 : p2 : gatherMismatchPrefixes lms

-- TODO possibly store prefix errors in ascending order in the Layout to speed this up
testMismatchPrefixes :: Prefix -> [Prefix] -> Bool
testMismatchPrefixes p = all $ \tp ->
  case joinAndCompare p tp of
    Right LT -> True
    _ -> False

hasMismatch :: Layout -> Bool
hasMismatch l =
  case gatherMismatches l of
    Empty -> False
    _ -> True

testMismatch :: Prefix -> Layout -> Bool
testMismatch p l = testMismatchPrefixes p (gatherMismatchPrefixes . gatherMismatches $ l)

-- TODO we should probably be using something like revAppendCat :: Relative a => Cat a -> Cat a -> Cat a
-- in many of the places this is appearing
revCat :: Relative a => Cat a -> Cat a
revCat Empty = Empty
revCat (x :< xs) = snocCat (revCat xs) x

runSnocMismatch :: LayoutMismatch -> Run -> Run
runSnocMismatch e (Run p ds ts es pr) = Run p ds ts (snocCat es e) pr

layoutError :: Prefix -> Prefix -> Run -> Run
layoutError pr p' = runSnocMismatch (LayoutMismatch 0 pr p')

mergeRun :: Delta -> Run -> Run -> Run
mergeRun d (Run p ds ts es _) (Run _ ds' ts' es' pr') =
  Run p (ds <> rel d ds') (ts <> rel d ts') (es <> rel d es') pr'

shuffle :: Delta -> Run -> Cat Run -> Maybe Run -> Rev Cat Run -> (Run, Cat Run, Maybe Run, Rev Cat Run)
shuffle d m l' m' r' =
  let
    (m'', l''', m''', r''') = shuffle' d m l' m' (revCat (runRev r'))
  in
    (m'', l''', m''', Rev . revCat $ r''')

shuffle' :: Delta -> Run -> Cat Run -> Maybe Run -> Cat Run -> (Run, Cat Run, Maybe Run, Cat Run)
shuffle' d m@(Run p _ ts _ pr) l'@(lh'@(Run p' _ _ Empty _) :< lt') m' r' =
  case (joinAndCompare p p', joinAndCompare pr p') of
    (Right LT, Right _) | boring ts -> shuffle' d (mergeRun d m lh') lt' m' r'
    (Left _, Right LT) | boring ts -> shuffle' d (mergeRun d m lh') lt' m' r'
    (Left _, _) -> (m, layoutError pr p' lh' :< lt', m', r')
    (_, Left _) -> (m, layoutError pr p' lh' :< lt', m', r')
    _ -> (m, l', m', r')
shuffle' d m@(Run p _ ts _ pr) Empty (Just m'@(Run p' _ _ Empty _)) r' =
  case (joinAndCompare p p', joinAndCompare pr p') of
    (Right LT, Right _) | boring ts -> shuffle' d (mergeRun d m m') Empty Nothing r'
    (Left _, Right LT) | boring ts -> shuffle' d (mergeRun d m m') Empty Nothing r'
    (Left _, _) -> (m, Empty, Just (layoutError pr p' m'), r')
    (_, Left _) -> (m, Empty, Just (layoutError pr p' m'), r')
    _ -> (m, Empty, Just m', r')
shuffle' d m@(Run p _ ts _ pr) Empty Nothing r'@(rh'@(Run p' _ _ Empty _) :< rt') =
  case (joinAndCompare p p', joinAndCompare pr p') of
    (Right LT, Right _) | boring ts -> shuffle' d (mergeRun d m rh') Empty Nothing rt'
    (Left _, Right LT) | boring ts -> shuffle' d (mergeRun d m rh') Empty Nothing rt'
    (Left _, _) -> (m, Empty, Nothing, layoutError pr p' rh' :< rt')
    (_, Left _) -> (m, Empty, Nothing, layoutError pr p' rh' :< rt')
    _ -> (m, Empty, Nothing, r')
shuffle' _ m Empty Nothing Empty =
  (m, Empty, Nothing, Empty)
shuffle' _ m l' m' d' =
  (m, l', m', d')

collapse :: Layout -> Layout
collapse (V d Empty m Empty) = S d m
collapse x = x

postfix :: Run -> Prefix
postfix (Run _ _ _ _ pr) = pr

leftmost :: Cat Run -> Run -> Run
leftmost Empty m = m
leftmost (l :< _) _ = l

rightmost :: Run -> Rev Cat Run -> Run
rightmost m Empty = m
rightmost _ (_ :> r) = r

instance Semigroup Layout where
  E 0 <> xs = xs
  xs <> E 0 = xs
  E d <> E d' = E (d <> d')
  E d <> S d' (Run p ds ts es pr) = S (d <> d') $ Run p (rel d ds) (rel d ts) (rel d es) pr
  E d <> V d' l m r = V (d <> d') (rel d l) (rel d m) (rel d r)
  S d m <> E d' = S (d <> d') m
  S d m@(Run p _ _ _ pr) <> S d' m'@(Run p' _ _ _ _) =
    case joinAndCompare p p' of
      Left _ ->
        V (d <> d') Empty m (rel d . Rev . Cat.singleton . layoutError pr p' $ m')
      Right GT ->
        V (d <> d') (Cat.singleton m) (rel d m') Empty
      _ ->
        let (m_, _, m'_, _) = shuffle d m Empty (Just m') Empty
        in collapse $ V (d <> d') Empty m_ (rel d . Rev . maybe Empty Cat.singleton $ m'_)

  S d m@(Run p _ _ _ pr) <> V d' l' m' r' =
    case joinAndCompare p (prefix m') of
      Left _ ->
        V (d <> d') Empty m . rel d $ case preview _Cons l' of
          Nothing ->
            (Rev . Cat.singleton . layoutError pr (prefix m') $ m') <> r'
          Just (lh', lt') ->
            (Rev . Cat.singleton . layoutError pr (prefix lh') $ lh') <> (Rev . revCat $ lt') <> (Rev . Cat.singleton $ m') <> r'
      Right GT ->
        let (m_, l'_, _, _) = shuffle d m l' Nothing Empty
        in V (d <> d') (m_ :< rel d l'_) (rel d m') (rel d r')
      _ ->
        let (m_, l'_, m'_, r'_) = shuffle d m l' (Just m') r'
        in collapse $ V (d <> d') Empty m_ (rel d (Rev (revCat l'_) <> Rev (maybe Empty Cat.singleton m'_) <> r'_))

  V d l m r <> E d' = V (d <> d') l m r

  V d l m r <> S d' m'@(Run p' _ _ _ _) =
    let pr = postfix (rightmost m r)
    in case (joinAndCompare (prefix m) p', joinAndCompare pr p') of
      (_, Left _) ->
         V (d <> d') l m (r :> (rel d . layoutError pr p' $ m'))
      (Right GT, _) | testMismatch p' (V d l m r) ->
        V (d <> d') (l <> Cat.singleton m <> revCat (runRev r)) (rel d m') Empty
      _ ->
        let
          (m_, r_, m'_) = case preview _Snoc r of
            Nothing ->
              let (m__, _, m'__, _) = shuffle d m Empty (Just m') Empty
              in (m__, Empty, m'__)
            Just (rt, rh) ->
              let (rh_, _, m'__, _) = shuffle d rh Empty (Just m') Empty
              in (m, rt :> rh_, m'__)
        in
          V (d <> d') l m_ (r_ <> (rel d . Rev . maybe Empty Cat.singleton $ m'_))

  V d l m@(Run p _ _ _ pr) Empty <> V d' Empty m'@(Run p' _ _ _ _) r' =
    case (joinAndCompare p p', joinAndCompare pr p') of
      (Left _, Left _) ->
        V (d <> d') l m (rel d (Rev (Cat.singleton . layoutError pr p' $ m') <> r'))
      (Right GT, _) ->
        V (d <> d') (l <> Cat.singleton m) (rel d m') (rel d r')
      _ ->
        let (m_, _, m'_, r'_) = shuffle d m Empty (Just m') r'
        in V (d <> d') l m_ (rel d (Rev (maybe Empty Cat.singleton m'_) <> r'_))

  V d l m r@(rt :> rh@(Run _ _ _ _ pr)) <> V d' Empty m'@(Run p' _ _ _ _) r' =
    case (joinAndCompare (prefix m) p', joinAndCompare pr p') of
      (Left _, Left _) ->
        V (d <> d') l m (r <> rel d (Rev (Cat.singleton . layoutError pr p' $ m') <> r'))
      (Right GT, _) | testMismatch p' (V d l m r) ->
        V (d <> d') (l <> Cat.singleton m <> revCat (runRev r)) (rel d m') (rel d r')
      _ ->
         let (rh_, _, m'_, r'_) = shuffle d rh Empty (Just m') r'
         in V (d <> d') l m ((rt :> rh_) <> rel d (Rev (maybe Empty Cat.singleton m'_) <> r'_))

  V d l m@(Run p _ _ _ pr) Empty <> V d' l'@(lh'@(Run p' _ _ _ _) :< lt') m' r' =
    case (joinAndCompare p (prefix m'), joinAndCompare pr p') of
      (Left _, Left _) ->
        V (d <> d') l m (Rev (Cat.singleton . layoutError pr p' $ lh') <> Rev (revCat lt') <> rel d (Rev (Cat.singleton m') <> r'))
      (Right GT, _) ->
        let (m_, l'_, _, _) = shuffle d m l' Nothing Empty
        in V (d <> d') (l <> Cat.singleton m_ <> rel d l'_) (rel d m') (rel d r')
      _ ->
        let (m_, l'_, m'_, r'_) = shuffle d m l' (Just m') r'
        in V (d <> d') l m_ (rel d (Rev (revCat l'_) <> Rev (maybe Empty Cat.singleton m'_) <> r'_))

  V d l m r@(rt :> rh@(Run _ _ _ _ pr)) <> V d' l'@(lh'@(Run p' _ _ _ _) :< lt') m' r' =
    case (joinAndCompare (prefix m) (prefix m'), joinAndCompare pr p') of
      (Left _, Left _) ->
        V (d <> d') l m (r <> rel d (Rev (Cat.singleton . layoutError pr p' $ lh') <> Rev (revCat lt') <> Rev (Cat.singleton m') <> r'))
      (Right GT, Right _) | testMismatch (prefix m') (V d l m r) ->
        let (rh_, _, lh'_, lt'_) = shuffle' d rh Empty (Just lh') lt'
        in V (d <> d') (l <> Cat.singleton m <> revCat (runRev rt) <> Cat.singleton rh_ <> rel d (maybe Empty Cat.singleton lh'_) <> rel d lt'_) (rel d m') (rel d r')
      (Right GT, Left _) | testMismatch (prefix m') (V d l m r) ->
        V (d <> d') (l <> Cat.singleton m <> revCat (runRev r) <> (rel d ((Cat.singleton . layoutError pr p' $ lh') <> lt'))) (rel d m') (rel d r')
      _ ->
        let (rh_, l'_, m'_, r'_) = shuffle d rh l' (Just m') r'
        in V (d <> d') l m ((rt :> rh_) <> rel d (Rev (revCat l'_) <> Rev (maybe Empty Cat.singleton m'_) <> r'_))

instance Monoid Layout where
  mempty = E 0
  mappend = (<>)
