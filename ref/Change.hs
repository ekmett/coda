{-# language CPP #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
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
import Control.Monad (guard, unless)
import Control.Monad.Fail
import Data.Foldable (fold)
import Data.Semigroup
import Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Unsafe
import Prelude hiding (fail)

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

consText :: Text -> FingerTree Text -> FingerTree Text
consText a as
  | Text.null a = as
  | otherwise = a :< as

snocText :: FingerTree Text -> Text -> FingerTree Text
snocText as a
  | Text.null a = as
  | otherwise = as :> a

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

-- not an inverse semigroup
invGrade :: Grade -> Grade
invGrade (Grade a b) = Grade b a

idGrade :: Delta -> Grade 
idGrade d = Grade d d 

composeGrade :: Alternative f => Grade -> Grade -> f Grade
composeGrade (Grade b' c) (Grade a b) = Grade a c <$ guard (b == b')

--------------------------------------------------------------------------------
-- Single edits
--------------------------------------------------------------------------------

-- Edits generate Change
data Edit = Edit !Delta !(FingerTree Text) !(FingerTree Text) -- requirement and replacement
  deriving (Show)

instance Eq Edit where
  Edit b as bs == Edit c cs ds = b == c && chunky as == chunky cs && chunky bs == chunky ds

instance Ord Edit where
  Edit b as bs `compare` Edit c cs ds = compare b c <> compare (chunky as) (chunky cs) <> compare (chunky bs) (chunky ds)

instance Measured Edit where
  type Measure Edit = Grade
  measure (Edit n f t) = Grade (n + delta f) (n + delta t)

-- @delta = delta . measure@
instance HasDelta Edit where
  delta (Edit n f _) = n + delta f 

instance HasOrderedDelta Edit

-- measure (invEdit e) = invGrade (measure e)
invEdit :: Edit -> Edit
invEdit (Edit d f t) = Edit d t f

instance Relative Edit where
  rel d (Edit n f t) = Edit (d+n) f t

partial :: MonadFail m => Partial a -> m a
partial (Left e) = fail e
partial (Right a) = pure a

censor :: Partial b -> Maybe b
censor = partial

type Partial = Either String

die :: String -> Partial a
die = Left

-- | @
-- censor (editDelta e >=> editDelta (invEdit e) >=> editDelta e) = censor (editDelta e)
-- censor (editDelta (invEdit e) >=> editDelta e >=> editDelta (invEdit e)) = censor (editDelta (invEdit e))
-- @
editDelta :: Edit -> Delta -> Partial Delta
editDelta (Edit n _ _) i
  | i < 0 = die "negative index"
  | i < n = pure i
  | otherwise = die "index too large"
  
stripSuffixes :: FingerTree Text -> Text -> Partial Text
stripSuffixes (xs :> x) y = case stripSuffix x y of
  Just y' -> stripSuffixes xs y'
  Nothing -> die $ show x ++ " is not a suffix of " ++ show y
stripSuffixes _ y = pure y

-- | @
-- censor (editText e >=> editText (invEdit e) >=> editText e) = censor (editText e)
-- censor (editText (invEdit e) >=> editText e >=> editText (invEdit e)) = censor (editText (invEdit e))
-- @
editText :: Edit -> Text -> Partial Text
editText (Edit n f t) s = do
  unless (delta s - delta f == n) $ die $ "editText: precondition failed: " ++ show (s, f, n)
  s' <- stripSuffixes f s
  let r = s <> fold t
  unless (delta r - delta t == n) $ die $ "editText: postcondition failed: " ++ show (r, t, n)
  pure r

--------------------------------------------------------------------------------
-- Multiple edits
--------------------------------------------------------------------------------

-- |
-- Invariants: 
--
-- 1) no two edits with 0 spaces between them. they get coalesced into a single edit node
-- 2) all edits have one of the finger-trees non-empty
data Change
  = Change !(FingerTree Edit) !Delta
  deriving (Eq,Ord,Show)

--instance Show Change where
--  show e = case ppChange e of (x,y,z) -> x ++ "\n" ++ y ++ "\n" ++ z

instance Relative Change where
  rel d (Change (e :< es) d') = Change (rel d e :< es) d'
  rel d (Change EmptyTree d') = Change EmptyTree (d+d')

-- | This measures the size of the domain, @delta (invChange d)@ measures the codomain
instance HasDelta Change where
  delta (Change xs d) = delta (measure xs) + d

instance Measured Change where
  type Measure Change = Grade
  measure (Change xs d) = rel d (measure xs) -- abuses commutativity

invChange :: Change -> Change
invChange (Change xs d) = Change (fmap' invEdit xs) d

concatEdits :: FingerTree Edit -> Delta -> FingerTree Edit -> Delta -> Change
concatEdits EmptyTree 0 ys e = Change ys e
concatEdits EmptyTree d EmptyTree e = Change EmptyTree (d+e)
concatEdits xs d EmptyTree e = Change xs (d+e)
concatEdits EmptyTree d (t :< ys) e = Change (rel d t <| ys) e
concatEdits (xs :> Edit n as bs) 0 (Edit 0 cs ds :< ys) e = Change ((xs |> Edit n (as <> cs) (bs <> ds)) <> ys) e
concatEdits xs d (t :< ys) e = Change ((xs |> rel d t) <> ys) e

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
changeDelta :: Change -> Delta -> Partial Delta
changeDelta (Change xs d) i = case search (\m _ -> i < delta m) xs of
  Position l _ _ | Grade o n <- measure l -> pure (n + i - o)
  OnRight
    | Grade o n <- measure xs, res <- i - o, res <= d -> Right (n + res)
    | otherwise -> die "changePos: Past end"
  OnLeft -> die "changePos: index < 0"
  Nowhere -> die "changePos: Nowhere"
  
changeText :: Change -> Text -> Partial Text
changeText c@(Change xs d) t
  | o <- delta xs, delta t == o + d = (<> dropDelta o t) <$> runApp (foldMapWithPos step xs)
  | otherwise = die $ "changeText: " ++ show (c,t)
  where step g e = App $ editText e $ takeDelta (delta e) $ dropDelta (delta g) t

class FromEdit a where
  edit :: Edit -> a

instance FromEdit Change where
  edit e 
    | Grade 0 0 <- measure e = Change mempty 0
    | otherwise = Change (FingerTree.singleton e) 0

instance FromEdit Edit where
  edit = id

inss :: FromEdit a => FingerTree Text -> a
inss xs = edit (Edit 0 mempty xs)

dels :: FromEdit a => FingerTree Text -> a
dels xs = edit (Edit 0 xs mempty)

ins :: FromEdit a => Text -> a
ins = inss . FingerTree.singleton

del :: FromEdit a => Text -> a
del = dels . FingerTree.singleton

cpy :: Delta -> Change
cpy = Change mempty

-- pretty printing for debugging

class Pretty a where
  pp :: a -> (String, String, String)

ppBar :: (String, String, String)
ppBar = ("|","|","|")

instance Pretty Delta where
  pp (units -> d) =
    ( Prelude.replicate d '∧'
    , Prelude.replicate d '|'
    , Prelude.replicate d '∨'
    )

flop :: Bool -> a -> a -> a -> (a, a, a)
flop False x y z = (x,y,z)
flop True x y z = (z,y,x)

pad :: Delta -> String
pad n = Prelude.replicate (units n) ' '

instance Pretty Edit where
  pp (Edit b f t)
    | d <- delta f, e <- delta t, c <- max e d 
    = pp b <> ppBar <> ( foldMap unpack f <> pad (c - d), pad c, foldMap unpack t <> pad (c - e))

instance Pretty Change where
  pp (Change xs d) = foldMap (\x -> pp x <> ppBar) xs <> pp d

pretty :: Pretty a => a -> IO ()
pretty e = traverseOf_ each putStrLn (pp e)

--------------------------------------------------------------------------------
-- everything from here down is probably broken!
--------------------------------------------------------------------------------

-- |
-- the middle result is an under-determined 'del' that can be slid freely between the two changes
--
-- @c = case splitChange d c of (l, m, r) -> l <> dels m <> r@
splitChange :: Delta -> Change -> (Change, FingerTree Text, Change)
splitChange i c@(Change xs d) = case search (\m _ -> i <= delta m) xs of
  Nowhere -> error "splitChange: Nowhere"
  OnLeft -> (mempty, mempty, c)
  OnRight | i' <- i - delta xs -> (Change xs i', mempty, cpy (d-i))
  Position l (Edit n f t) r 
    | j < n -> (Change l j, mempty, Change (Edit (n-j) f t <| r) d)
    | otherwise -> case search (\m _ -> j <= delta m) f of
      Nowhere -> error "splitChange: Nowhere(2)"
      OnLeft -> (Change l n, t, Change (Edit 0 f mempty <| r) d) -- j == n
      OnRight -> error "splitChange: OnRight(2)"
      Position fl (splitDelta (j - delta fl) -> (fml, fmr)) fr ->
        (Change (l :> Edit n (fl |> fml) mempty) 0, t, Change (Edit 0 (fmr <| fr) mempty :< r) d) 
    where j = i - delta l

-- | @
-- censor (editChange e >=> editChange (invEdit e) >=> editChange e) = censor (editChange e)
-- censor (editChange (invEdit e) >=> editChange e >=> editChange (invEdit e)) = censor (editChange (invEdit e))
-- @
editChange :: Edit -> Change -> Partial Change
editChange (Edit d f t) c = case splitChange d c of
  (l,md,r) -> do
    f' <- changeText r (fold f)
    pure $ l <> dels md <> del f' <> inss t
    
{-

takeChange, dropChange :: Delta -> Change -> Change
takeChange d c = fst $ splitChange d c
dropChange d c = snd $ splitChange d c

-- compose changes in series
changeChange :: Change -> Change -> Partial Change
changeChange (Change xs d) t 
  | o <- delta xs, delta t == o + d = (<> dropChange o t) <$> runApp (foldMapWithPos step xs)
  | otherwise = Left "changeText: wrong length"
  where
    step g e = App $ editChange e $ takeChange (delta e) $ dropChange (delta g) t

-- validate that this change can be applied to this text at the end and compute the left hand derivative
stripChangeSuffix :: Text -> Change -> Partial Change
stripChangeSuffix t c 
  | n <- delta t - delta c, n >= 0, (l,r) <- splitChange n c = l <$ changeText r t
  | otherwise = Left "stripChangeSuffix: suffix too long to match"

stripChangeSuffixes :: FingerTree Text -> Change -> Partial Change
stripChangeSuffixes (xs :> x) y = stripChangeSuffix x y >>= stripChangeSuffixes xs
stripChangeSuffixes _ y = Right y

-- try to compose changes
composeChange :: Change -> Change -> Maybe Change
composeChange xs ys = censor (changeChange xs ys)

-}
