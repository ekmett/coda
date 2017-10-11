{-# language CPP #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ < 802
{-# options_ghc -Wno-incomplete-patterns #-}
#endif

import Coda.FingerTree as FingerTree
import Coda.Relative.Class
import Coda.Relative.Delta
import Control.Applicative
import Control.Lens
import Control.Monad (unless)
import Data.Foldable (fold)
import Data.Semigroup
import Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Unsafe

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

chunky :: Foldable f => f Text -> Lazy.Text
chunky = Lazy.fromChunks . foldMap pure
{-# inline chunky #-}

foldMapWithPos :: forall a m. (Measured a, Monoid m) => (Measure a -> a -> m) -> FingerTree a -> m
foldMapWithPos f = getConst . traverseWithPos (\v a -> Const (f v a) :: Const m (FingerTree a))

takeDelta, dropDelta :: Delta -> Text -> Text
takeDelta = takeWord16 . units
dropDelta = dropWord16 . units

splitDelta :: Delta -> Text -> (Text, Text)
splitDelta d t = (takeDelta d t, dropDelta d t)

--------------------------------------------------------------------------------
-- Grading Changes
--------------------------------------------------------------------------------

-- old size, new size
data Grade = Grade !Delta !Delta
  deriving (Eq,Ord,Show,Read)

instance Monoid Grade where
  mempty = Grade 0 0
  mappend (Grade a b) (Grade c d) = Grade (a + c) (b + d)

-- size of the domain
instance HasDelta Grade where
  delta (Grade o _) = o

instance Relative Grade where
  rel d (Grade o n) = Grade (d+o) (d+n)

instance HasMonoidalDelta Grade
instance HasOrderedDelta Grade

instance Num Grade where
  Grade a b + Grade c d = Grade (a + c) (b + d)
  Grade a b - Grade c d = Grade (a - c) (b - d)
  Grade a b * Grade c d = Grade (a * c) (b * d)
  negate (Grade a b) = Grade (negate a) (negate b)
  abs (Grade a b) = Grade (abs a) (abs b)
  signum (Grade a b) = Grade (signum a) (signum b)
  fromInteger a = Grade (fromInteger a) (fromInteger a)

--------------------------------------------------------------------------------
-- Single edits
--------------------------------------------------------------------------------

-- Edits generate Change
data Edit
  = Insert !Delta !(FingerTree Text) -- inserted text
  | Delete !Delta !(FingerTree Text) -- deleted text
  deriving (Show)

instance Eq Edit where
  Delete d xs == Delete e ys = d == e && chunky xs == chunky ys
  Insert d xs == Insert e ys = d == e && chunky xs == chunky ys
  _ == _ = False

instance Ord Edit where
  compare (Delete d xs) (Delete e ys) = compare d e <> compare (chunky xs) (chunky ys)
  compare (Insert d xs) (Insert e ys) = compare d e <> compare (chunky xs) (chunky ys)
  compare Insert{} Delete{} = LT
  compare Delete{} Insert{} = LT

instance Measured Edit where
  type Measure Edit = Grade
  measure (Insert n t) = Grade n (n + delta t)
  measure (Delete n t) = Grade (n + delta t) n

-- @delta = delta . measure@
instance HasDelta Edit where
  delta (Insert n _) = n
  delta (Delete n t) = n + measure t

instance HasOrderedDelta Edit

invEdit :: Edit -> Edit
invEdit (Insert n t) = Delete n t
invEdit (Delete n t) = Insert n t

instance Relative Edit where
  rel d (Insert n t) = Insert (d+n) t
  rel d (Delete n t) = Delete (d+n) t

censor :: Either a b -> Maybe b
censor = either (const Nothing) Just

-- | @
-- modulo the exact text of the helpful error
--
-- censor (editDelta e >=> editDelta (invEdit e) >=> editDelta e) = censor (editDelta e)
-- censor (editDelta (invEdit e) >=> editDelta e >=> editDelta (invEdit e)) = censor (editDelta (invEdit e))
-- @
--
editDelta :: Edit -> Delta -> Either String Delta
editDelta (Insert n t) i
  | i < 0 = Left "negative index"
  | i < n = Right i
  | i < n + nt = Right (i + nt)
  | otherwise = Left "index too large"
   where nt = measure t
editDelta (Delete n _) i
  | i < 0 = Left "negative index"
  | i < n = Right i
  | otherwise = Left "index too large"
  
stripSuffixes :: FingerTree Text -> Text -> Either String Text
stripSuffixes (xs :> x) y = case stripSuffix x y of
  Just y' -> stripSuffixes xs y'
  Nothing -> Left (show x ++ " is not a suffix of " ++ show y)
stripSuffixes _ y = Right y

-- | @
-- censor (editText e >=> editText (invEdit e) >=> editText e) = censor (editText e)
-- censor (editText (invEdit e) >=> editText e >=> editText (invEdit e)) = censor (editText (invEdit e))
-- @
editText :: Edit -> Text -> Either String Text
editText (Insert n s) t = do
  unless (n == delta t) $ Left ("insert: expected " ++ show n ++ " characters, but was given: " ++ show t)
  pure (t <> fold s)
editText (Delete n s) t = do
  r <- stripSuffixes s t
  unless (n == delta r) $ Left ("delete: expected " ++ show n ++ " characters, but was given: " ++ show r) 
  pure r

--------------------------------------------------------------------------------
-- Multiple edits
--------------------------------------------------------------------------------

-- |
-- Invariants: 
--
-- 1) no two inserts with 0 spaces between them. they get coalesced into a single insert node
--
-- 2) no two deletes with 0 spaces between them. they get coalesced into a single delete node
--
-- 3) no deletes followed by an insert that re-inserts the same prefix we just deleted
--
-- 4) no inserts followed by a delete that deletes the same prefix we just deleted
--
-- 5) all inserts and deletes have non-empty finger-trees of text fragments
data Change
  = Change !(FingerTree Edit) !Delta
  deriving (Eq,Ord,Show)

instance Relative Change where
  rel d (Change (e :< es) d') = Change (rel d e :< es) d'
  rel d (Change EmptyTree d') = Change EmptyTree (d+d')

-- | This measures the size of the domain, @delta (invChange d)@ measures the codomain
instance HasDelta Change where
  delta (Change xs d) = delta (measure xs) + d

instance Measured Change where
  type Measure Change = Grade
  measure (Change xs d) = rel d (measure xs) -- uses commutativity

-- the identity change on a text n words long
idChange :: Delta -> Change
idChange = Change mempty

invChange :: Change -> Change
invChange (Change xs d) = Change (fmap' invEdit xs) d

commonPrefixLength :: Text -> Text -> Maybe (Delta, Maybe Text, Maybe Text)
commonPrefixLength xs ys = do
  (as,bs,cs) <- commonPrefixes xs ys
  pure (delta as, if Text.null bs then Nothing else Just bs, if Text.null bs then Nothing else Just cs)

data Collapse
  = Neither !Delta
  | EitherLeft !Delta !(FingerTree Text)
  | EitherRight !Delta !(FingerTree Text)
  | Both !Delta !(FingerTree Text) !(FingerTree Text)

collapseT :: Delta -> FingerTree Text -> FingerTree Text -> Collapse
collapseT n0 (s0 :< xs0) (t0 :< ys0) = go n0 s0 xs0 t0 ys0 where
  go n s xs t ys = case commonPrefixLength s t of
    Nothing -> Both n (s :< xs) (t :< ys) -- no common prefix
    Just (r, Nothing, Nothing) -> case xs of
      EmptyTree -> case ys of
        EmptyTree -> Neither (n+r)
        _ -> EitherRight (n+r) ys
      s' :< xs' -> case ys of
        EmptyTree -> EitherLeft (n+r) xs
        t' :< ys' -> go (n+r) s' xs' t' ys'
    Just (r, Nothing, Just t') -> case xs of
      EmptyTree -> EitherRight (n+r) (t' :< ys)
      s' :< xs' -> go (n+r) s' xs' t' ys
    Just (r, Just s', Nothing) -> case ys of
      EmptyTree -> EitherLeft (n+r) (s' :< xs)
      t' :< ys' -> go (n+r) s' xs t' ys'
    Just (r, Just s', Just t') -> Both (n+r) (s' :< xs) (t' :< ys)
collapseT _ _ _ = error "collapseN: invariant 5 violated"
  
collapse :: FingerTree Edit -> Edit -> Edit -> FingerTree Edit -> Delta -> Change
collapse xs (Insert n as) (Insert 0 bs) ys d = Change ((xs |> Insert n (as <> bs)) <> ys) d -- restore invariant 1
collapse xs (Delete n as) (Delete 0 bs) ys d = Change ((xs |> Delete n (as <> bs)) <> ys) d -- restore invariant 2
collapse xs (Insert n as) (Delete 0 bs) ys d = case collapseT n as bs of -- restore invariant 4
  Neither n' -> concatEdits xs n' ys d -- invariant 5
  Both n' as' bs' -> Change (((xs |> Insert n' as') |> Delete 0 bs') <> ys) d -- no common prefix
  EitherLeft n' as' -> case ys of
    Insert 0 bs' :< ys' -> Change ((xs |> Insert n' (as' <> bs')) <> ys') d -- restore invariant 1
    _ -> Change ((xs |> Insert n' as) <> ys) d
  EitherRight 0 bs' -> case xs of
    xs' :> Delete n' as' -> Change ((xs' |> Delete n' (as' <> bs')) <> ys) d -- restore invariant 2
    _ -> Change ((xs |> Delete 0 as) <> ys) d
  EitherRight n' bs' -> Change ((xs |> Delete n' bs') <> ys) d
collapse xs (Delete n as) (Insert 0 bs) ys d = case collapseT n as bs of -- restore invariant 3
  Neither n' -> concatEdits xs n' ys d
  Both n' as' bs' -> Change (((xs |> Delete n' as') |> Insert 0 bs') <> ys) d -- no common prefix
  EitherLeft n' as' -> case ys of
    Delete 0 bs' :< ys' -> Change ((xs |> Delete n' (as' <> bs')) <> ys') d -- restore invariant 2
    _ -> Change ((xs |> Delete n' as) <> ys) d
  EitherRight 0 bs' -> case xs of
    xs' :> Insert n' as' -> Change ((xs' |> Insert n' (as' <> bs')) <> ys) d -- restore invariant 1
    _ -> Change ((xs |> Insert 0 as) <> ys) d
  EitherRight n' bs' -> Change ((xs |> Insert n' bs') <> ys) d
collapse xs s t ys d = Change (((xs |> s) |> t) <> ys) d

concatEdits :: FingerTree Edit -> Delta -> FingerTree Edit -> Delta -> Change
concatEdits EmptyTree 0 ys        e = Change ys e
concatEdits EmptyTree d EmptyTree e = Change EmptyTree (d+e)
concatEdits xs        d EmptyTree e = Change xs (d+e)
concatEdits EmptyTree d (t :< ys) e = Change (rel d t <| ys) e
concatEdits (xs :> s) 0 (t :< ys) e = collapse xs s t ys e
concatEdits xs        d (t :< ys) e = Change ((xs |> rel d t) <> ys) e

-- | given a change x that will successfully apply to t, and a change y that successfully applies to s
-- concatChange x y successfully applies to (t <> s)
instance Semigroup Change where
  Change EmptyTree 0 <> rhs = rhs
  lhs <> Change EmptyTree 0 = lhs
  Change xs d <> Change ys e = concatEdits xs d ys e
  
instance Monoid Change where
  mempty = Change EmptyTree 0
  mappend = (<>)
    
newtype App f a = App { runApp :: f a } deriving (Functor,Applicative)

instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | /O(log(min(k,n-k)))/ where there are @n@ edits, @k@ of which occur before the position in question
-- 
-- @
-- censor (changeDelta e >=> changeDelta (invChange e) >=> changeDelta e) = censor (changeDelta e)
-- censor (changeDelta (invChange e) >=> changeDelta e >=> changeDelta (invChange e)) = censor (changeDelta (invChange e))
-- @
changeDelta :: Change -> Delta -> Either String Delta
changeDelta (Change xs d) i = case search (\m _ -> i < delta m) xs of
  Position l e _ | Grade o n <- measure l -> (n +) <$> editDelta e (i - o)
  OnRight
    | Grade o n <- measure xs, res <- i - o, res <= d -> Right (n + res)
    | otherwise -> Left "changePos: Past end"
  OnLeft -> Left "changePos: index < 0"
  Nowhere -> Left "changePos: Nowhere"
  
changeText :: Change -> Text -> Either String Text
changeText (Change xs d) t 
  | o <- delta xs
  , delta t == o + d
  = (<> dropDelta o t) <$> runApp (foldMapWithPos step xs)
  | otherwise = Left "changeText: wrong length"
  where step g e = App $ editText e $ takeDelta (delta e) $ dropDelta (delta g) t
 

class FromEdit a where
  edit :: Edit -> a

instance FromEdit Edit where
  edit = id

instance FromEdit Change where
  edit e = Change (FingerTree.singleton e) 0

ins :: Text -> Change
ins = edit . Insert 0 . snocText mempty

del :: Text -> Change
del = edit . Delete 0 . snocText mempty

cpy :: Delta -> Change
cpy = Change mempty

consText :: Text -> FingerTree Text -> FingerTree Text
consText a as
  | Text.null a = as
  | otherwise = a :< as

snocText :: FingerTree Text -> Text -> FingerTree Text
snocText as a
  | Text.null a = as
  | otherwise = as :> a

--------------------------------------------------------------------------------
-- everything from here down is probably broken!
--------------------------------------------------------------------------------

splitEdits :: Delta -> FingerTree Edit -> Edit -> FingerTree Edit -> Delta -> (Change, Change)
splitEdits i xs (Insert n as) ys d = (Change xs i, Change (Insert (n-i) as <| ys) d)
splitEdits i xs (Delete n as) ys d
  | i <= n = (Change xs i, Change (Delete (n-i) as <| ys) d)
  | otherwise = case search (\m _ -> i < delta m) as of
    Position l m r
      | !j <- i - delta l
      , (ml, mr) <- splitDelta j m 
      -> ( Change (xs |> Delete n (snocText l ml)) 0, Change (Delete 0 (consText mr r) <| ys) d)
    _ -> error "splitEdits: logic error"
 
splitChange :: Delta -> Change -> (Change, Change)
splitChange i c@(Change xs d) = case search (\m _ -> i < delta m) xs of
  Position l e r -> splitEdits (i - delta l) l e r d
  OnLeft  -> (mempty, c)
  OnRight | i' <- i - delta xs -> (Change xs i', cpy (d-i))
  Nowhere -> error "splitChange: nowhere"

takeChange, dropChange :: Delta -> Change -> Change
takeChange d c = fst $ splitChange d c
dropChange d c = snd $ splitChange d c

-- compose changes in series
changeChange :: Change -> Change -> Either String Change
changeChange (Change xs d) t 
  | o <- delta xs, delta t == o + d = (<> dropChange o t) <$> runApp (foldMapWithPos step xs)
  | otherwise = Left "changeText: wrong length"
  where
    step g e = App $ editChange e $ takeChange (delta e) $ dropChange (delta g) t

-- | @
-- censor (editChange e >=> editChange (invEdit e) >=> editChange e) = censor (editChange e)
-- censor (editChange (invEdit e) >=> editChange e >=> editChange (invEdit e)) = censor (editChange (invEdit e))
-- @
editChange :: Edit -> Change -> Either String Change
editChange (Insert n s) t = do
  unless (n == delta t) $ Left ("insert: expected " ++ show n ++ " characters, but was given: " ++ show t)
  pure (t <> foldMap ins s)
editChange (Delete n s) t = do
  r <- stripChangeSuffixes s t
  unless (n == delta r) $ Left ("delete: expected " ++ show n ++ " characters, but was given: " ++ show r) 
  pure r

-- validate that this change can be applied to this text at the end and compute the left hand derivative
stripChangeSuffix :: Text -> Change -> Either String Change
stripChangeSuffix t c 
  | n <- delta t - delta c, n >= 0, (l,r) <- splitChange n c = l <$ changeText r t
  | otherwise = Left "stripChangeSuffix: suffix too long to match"

stripChangeSuffixes :: FingerTree Text -> Change -> Either String Change
stripChangeSuffixes (xs :> x) y = stripChangeSuffix x y >>= stripChangeSuffixes xs
stripChangeSuffixes _ y = Right y

-- try to compose changes
composeChange :: Change -> Change -> Maybe Change
composeChange xs ys = censor (changeChange xs ys)
