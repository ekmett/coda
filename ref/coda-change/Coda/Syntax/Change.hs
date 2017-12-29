{-# language EmptyCase #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language DefaultSignatures #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableSuperClasses #-}
{-# options_ghc -Wno-incomplete-patterns #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Coda.Syntax.Change where

import Coda.Algebra.Zero
import Coda.FingerTree as FingerTree
import Coda.Relative.Class
import Coda.Relative.Delta
import Control.Applicative as Applicative
import Control.Lens
import Control.Monad (MonadPlus(..), unless)
import Control.Monad.Fail (MonadFail(fail))
import Data.Default
import Data.Semigroup
import Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Unsafe
import Prelude hiding (fail)
import Text.Read

--------------------------------------------------------------------------------
-- Inverse Semigroups
--------------------------------------------------------------------------------

-- | @
-- a <> inv a <> a = a
-- inv a <> a <> inv a = inv a
-- @
--
-- and all idempotents commute and are of the form @(a <> inv a)@ for some @a@
class Semigroup a => InverseSemigroup a where
  inv :: a -> a

--------------------------------------------------------------------------------
-- Partiality
--------------------------------------------------------------------------------

-- Void viewed as having one undistinguishable inhabitant: an error message, no pure function can
-- distinguish its inhabitants, so we can ignore it in our semantics, but we can still give some
-- kind of non-deterministic diagnostic output

data Error
instance Eq Error where _ == _ = True
instance Ord Error where compare _ _ = EQ
instance Show Error where showsPrec _ !e = case e of {}
instance Read Error where
  readPrec = fail "Error"
  readListPrec = readListPrecDefault

-- | This occupies an uncomfortable middle ground between Maybe and Either
data Partial a
  = OK !a
  | Fail Error
  deriving (Eq, Ord, Read, Functor, Foldable, Traversable)

instance Show a => Show (Partial a) where
  showsPrec d (OK a) = showParen (d > 10) $ showString "OK " . showsPrec 11 a
  showsPrec _ (Fail !e) = case e of {}

instance Applicative Partial where
  pure = OK
  {-# inline pure #-}

  OK f <*> OK a = OK (f a)
  Fail e <*> _ = Fail e
  _ <*> Fail e = Fail e
  {-# inline (<*>) #-}

  OK{} *> m = m
  Fail e *> _ = Fail e
  {-# inline (*>) #-}

  m <* OK{} = m
  _ <* Fail e = Fail e
  {-# inline (<*) #-}

instance Alternative Partial where
  empty = fail "empty"
  {-# inline empty #-}
  Fail{} <|> m = m
  m <|> _ = m
  {-# inline (<|>) #-}

instance Monad Partial where
  OK a >>= f = f a
  Fail e >>= _ = Fail e
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

instance MonadPlus Partial where
  mzero = Applicative.empty
  {-# inline mzero #-}
  mplus = (<|>)
  {-# inline mplus #-}

instance MonadFail Partial where
  fail e = Fail (error e)
  {-# inline conlike fail #-}

-- trustme
total :: Partial a -> a
total (Fail !e) = case e of {}
total (OK a) = a
{-# inline total #-}

partial :: Partial a -> Maybe a
partial (Fail _) = Nothing
partial (OK a)   = Just a
{-# inline partial #-}

--------------------------------------------------------------------------------
-- Composable
--------------------------------------------------------------------------------

-- A partial semigroup
class Composable a where
  compose :: a -> a -> Partial a
  default compose :: Semigroup a => a -> a -> Partial a
  compose a b = OK (a <> b)

instance Composable a => Semigroup (Partial a) where
  OK a <> OK b = compose a b
  e@Fail{} <> _ = e
  _ <> e@Fail{} = e
  {-# inline (<>) #-}

instance Composable a => SemigroupWithZero (Partial a) where
  zero = fail "zero"

instance Composable Delta where
  compose a b
    | a == b    = pure a
    | otherwise = fail $ "compose Delta " ++ show a ++ " /= " ++ show b

--------------------------------------------------------------------------------
-- Inverse
--------------------------------------------------------------------------------

-- Makes 'Partial a' an inverse semigroup
class (Inverse (Idempotent a), Composable a, Idempotent (Idempotent a) ~ Idempotent a) => Inverse a where
  inverse :: a -> a

  -- | Inverse gives us an inverse category, but the 'types' aren't parameters and are down here at the value level
  --
  -- @src@ and @tgt@ are the source and target mappings, and @ident@ provides us the identity arrows.
  type Idempotent a :: *
  type Idempotent a = a

  -- a <> idd (src a) = a = idd (tgt a) <> a
  src :: a -> Idempotent a
  default src :: (Idempotent a ~ a) => a -> Idempotent a
  src a = total $ compose (inverse a) a

  tgt :: a -> Idempotent a
  default tgt :: (Idempotent a ~ a) => a -> Idempotent a
  tgt a = total $ compose a (inverse a)

  idd :: Idempotent a -> a
  default idd :: (Idempotent a ~ a) => Idempotent a -> a
  idd = id

instance Inverse a => InverseSemigroup (Partial a) where
  inv = fmap inverse
  {-# inline inv #-}

-- not to be confused with inv!
instance Inverse Delta where
  inverse a = a

--------------------------------------------------------------------------------
-- Text Utilities
--------------------------------------------------------------------------------

chunky :: Foldable f => f Text -> Lazy.Text
chunky = Lazy.fromChunks . foldMap pure
{-# inline chunky #-}

foldMapWithPos :: forall a m. (Measured a, Monoid m) => (Measure a -> a -> m) -> FingerTree a -> m
foldMapWithPos f = getConst . traverseWithPos (\v a -> Const (f v a) :: Const m (FingerTree a))
{-# inline foldMapWithPos #-}

-- respects measure
class (Measured t, HasDelta (Measure t)) => Splittable t where
  splitDelta :: Delta -> t -> (t, t)
  splitDelta d t = (takeDelta d t, dropDelta d t)
  {-# inline splitDelta #-}

  takeDelta, dropDelta :: Delta -> t -> t
  takeDelta d = fst . splitDelta d
  {-# inline takeDelta #-}

  dropDelta d = fst . splitDelta d
  {-# inline dropDelta #-}

  {-# minimal splitDelta | (takeDelta, dropDelta) #-}

instance Splittable Delta where
  splitDelta i j
    | i < 0 = (0,j)
    | i < j = (i,j - i)
    | otherwise = (j,0)

instance Splittable Text where
  takeDelta = takeWord16 . units
  dropDelta = dropWord16 . units
  {-# inline takeDelta #-}
  {-# inline dropDelta #-}

consText :: Text -> FingerTree Text -> FingerTree Text
consText a as
  | Text.null a = as
  | otherwise = a :< as
{-# inline consText #-}

snocText :: FingerTree Text -> Text -> FingerTree Text
snocText as a
  | Text.null a = as
  | otherwise = as :> a
{-# inline snocText #-}

instance Splittable a => Splittable (FingerTree a) where
  splitDelta i xs = case search (\m _ -> i <= delta m) xs of
    Position l (splitDelta (i - delta l) -> (el,er)) r -> (l :> el, er :< r) -- TODO: be more careful about empties
    OnLeft  -> (mempty,xs)
    OnRight -> (xs,mempty)
    Nowhere -> error "dropsDelta: Nowhere"

class Strippable a where
  stripSuffixes :: FingerTree Text -> a -> Partial a

instance Strippable Text where
  stripSuffixes (xs :> x) y = case stripSuffix x y of
    Just y' -> stripSuffixes xs y'
    Nothing -> fail $ show x ++ " is not a suffix of " ++ show y
  stripSuffixes _ y = pure y

instance Strippable (FingerTree Text) where
  stripSuffixes EmptyTree ys = pure ys
  stripSuffixes xs EmptyTree = fail $ "stripSuffix: empty rhs " ++ show xs
  stripSuffixes (xs0 :> x0) (ys0 :> y0) = go xs0 x0 ys0 y0 where
    go xs x ys y = case compare (delta x) (delta y) of
      EQ | x == y -> stripSuffixes xs ys
         | otherwise -> fail $ "stripSuffixes mismatch: " ++ show (x,y)
      LT -> case stripSuffix x y of
        Just y' -> case xs of
          EmptyTree -> pure $ snocText ys y'
          xs' :> x' -> go xs' x' ys y'
        Nothing -> fail $ "stripSuffixes: not a suffix: " ++ show (x, y)
      GT -> case stripSuffix y x of
        Just x' -> case ys of
          EmptyTree -> fail $ "stripSuffix: underflow" ++ show (xs,x')
          ys' :> y' -> go xs x' ys' y'
        Nothing -> fail $ "stripSuffixes: not a suffix (reverse): " ++ show (y, x)

--------------------------------------------------------------------------------
-- PP Printing Changes
--------------------------------------------------------------------------------

-- It is convenient to show two lines pairing inputs with outputs
--
-- This is a minimalist pretty printer for that format.
class PP a where
  pp :: a -> (String, String)

ppBar :: (String, String)
ppBar = ("|","|")

instance PP Delta where
  pp n = (pad n '∧', pad n '∨')

pad :: Delta -> Char -> String
pad = Prelude.replicate . units

pretty :: PP a => a -> IO ()
pretty e = traverseOf_ each putStrLn (pp e)

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

instance Composable Grade where
  compose (Grade b' c) (Grade a b) = Grade a c <$ unless (b == b') (fail "grade mismatch")

instance Inverse Grade where
  inverse (Grade a b) = Grade b a
  type Idempotent Grade = Delta
  src (Grade a _) = a
  tgt (Grade _ b) = b
  idd a = Grade a a

--------------------------------------------------------------------------------
-- Single edits
--------------------------------------------------------------------------------

-- Edits generate Change
data Edit = Edit !Delta !Delta !Delta -- requirement and replacement
  deriving (Eq, Ord, Show)

instance Default Edit where
  def = Edit 0 0 0

instance Measured Edit where
  type Measure Edit = Grade
  measure (Edit n f t) = Grade (n + f) (n + t)

-- @delta = delta . measure@
instance HasDelta Edit where
  delta (Edit n f _) = n + f

instance HasOrderedDelta Edit

-- measure (inverseEdit e) = invGrade (measure e)
inverseEdit :: Edit -> Edit
inverseEdit (Edit d f t) = Edit d t f

instance Relative Edit where
  rel d (Edit n f t) = Edit (d+n) f t

instance Splittable Edit where
  splitDelta i e@(Edit n f t)
    | i < 0 = (def, e)
    | i < n = (cpy i, Edit (n-1) f t)
    | nf <- n+f, i < nf = (Edit n (nf - i) t, Edit 0 (f + i - nf) 0)
    | otherwise = (e, def)

-- | @
-- edit e >=> edit (inverseEdit e) >=> edit e = edit e
-- edit (inverseEdit e) >=> edit e >=> edit (inverseEdit e) = edit (inverseEdit e)
-- @
class Editable a where
  edit :: Edit -> a -> Partial a

instance Editable Delta where
  edit (Edit n _ _) i
    | i < 0 = fail "negative index"
    | i < n = pure i
    | otherwise = fail "index too large"

instance PP Edit where
  pp (Edit b f t)
    | c <- max f t = pp b <> (pad f '-' <> pad (c-f) ' ', pad t '+' <> pad (c-t) ' ')

class FromEdit a where
  fromEdit :: Edit -> a

instance FromEdit Edit where
  fromEdit = id

ins :: FromEdit a => Delta -> a
ins n = fromEdit (Edit 0 0 n)

del :: FromEdit a => Delta -> a
del n = fromEdit (Edit 0 n 0)

cpy :: FromEdit a => Delta -> a
cpy n = fromEdit (Edit n 0 0)

--------------------------------------------------------------------------------
-- Multiple edits
--------------------------------------------------------------------------------

-- |
-- Invariants:
--
-- 1) no two edits with 0 spaces between them. they get coalesced into a single edit node
--
-- 2) all edits have at least one of the finger-trees non-empty
--
-- Changes are simplicial morphisms, monotone functions between finite sets of integers that start at 0
data Change = Change !(FingerTree Edit) !Delta deriving (Eq,Ord,Show)

{-# complete C0, CN #-}
{-# complete Change #-}

changePattern :: Change -> (FingerTree Edit, Delta)
changePattern (C0 d)      = (mempty, d)
changePattern (CN e es d) = (e :< es, d)

pattern C0 :: Delta -> Change
pattern C0 d = Change EmptyTree d

pattern CN :: Edit -> FingerTree Edit -> Delta -> Change
pattern CN x xs d = Change (x :< xs) d


instance Relative Change where
  rel d (C0 d')      = C0 (d+d')
  rel d (CN e es d') = CN (rel d e) es d'

-- | This measures the size of the domain, @delta (inv d)@ measures the codomain
instance HasDelta Change where
  delta (Change es d) = delta es + d

instance Measured Change where
  type Measure Change = Grade
  measure (Change es d) = measure es + Grade d d

instance Inverse Change where
  inverse (Change es d) = Change (fmap' inverseEdit es) d

-- | given a change x that will successfully apply to t, and a change y that successfully applies to s
-- concatChange x y successfully applies to (t <> s)
instance Semigroup Change where
  C0 0 <> rhs = rhs
  lhs <> C0 0 = lhs
  Change xs0 d0 <> Change ys0 e0 = go xs0 d0 ys0 e0 where
    go EmptyTree 0 ys e = Change ys e
    go EmptyTree d EmptyTree e = C0 (d+e)
    go xs d EmptyTree e = Change xs (d+e)
    go EmptyTree d (t :< ys) e = Change (rel d t <| ys) e
    go (xs :> Edit n as bs) 0 (Edit 0 cs ds :< ys) e = Change ((xs |> Edit n (as <> cs) (bs <> ds)) <> ys) e
    go xs d (t :< ys) e = Change ((xs |> rel d t) <> ys) e

instance Monoid Change where
  mempty = C0 0
  mappend = (<>)

newtype App f a = App { runApp :: f a } deriving (Functor,Applicative)

instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty = pure mempty
  mappend = liftA2 mappend

class Changeable a where
  change :: Change -> a -> Partial a

-- | /O(log(min(k,n-k)))/ where there are @n@ edits, @k@ of which occur before the position in question
instance Changeable Delta where
  change (Change xs d) i = case search (\m _ -> i < delta m) xs of
    Position (measure -> Grade o n) (Edit a _ _) _
      | i - o <= a -> pure (n + i - o)
      | otherwise  -> fail "changePos: deleted position"
    OnRight
      | Grade o n <- measure xs, res <- i - o, res < d -> pure (n + res)
      | otherwise -> fail "changePos: Past end"
    OnLeft -> fail "changePos: index < 0"
    Nowhere -> fail "changePos: Nowhere"

instance FromEdit Change where
  fromEdit e
    | Grade 0 0 <- measure e = C0 0
    | otherwise = Change (FingerTree.singleton e) 0

instance PP Change where
  pp (Change xs d) = foldMap pp xs <> pp d

-- |
-- @c = case splitChange d c of (l, r) -> l <> r
-- grade c = case splitChange d c of (l, r) -> grade l + grade r
-- delta (fst $ splitChange d c) = max 0 (min d (grade c))
-- delta (snd $ splitChange d c) = max 0 (min d (grade c - d))
-- @
--
-- O(log n)
instance Splittable Change where
  splitDelta i c@(Change xs d) = case search (\m _ -> i <= delta m) xs of
    Nowhere -> error "splitChange: Nowhere"
    OnLeft -> (mempty, c)
    OnRight | i' <- i - delta xs -> (Change xs i', cpy (d-i))
    Position l (Edit n f t) r
      | j < n -> (Change l j, Change (Edit (n-j) f t <| r) d)
      | (fl,fr) <- splitDelta (j - n) f -> (Change l n <> del fl <> ins t, del fr <> Change r d)
      where j = i - delta l

instance Editable Change where
  edit (Edit d f t) (splitDelta d -> (l,r)) = change r t <&> \t' -> l <> del f <> ins t'

-- | @change f g@ provides @g . f@
instance Changeable Change where
  change (Change xs0 d0) = go xs0 d0 where
    -- TODO: figure out an optimal split ordering by adding a cost to each edit
    go (e :< es) d (splitDelta (delta (inverseEdit e)) -> (l,r)) = (<>) <$> edit e l <*> go es d r
    go EmptyTree d c = do
      unless (delta c == d) $ fail $ "changeChange: leftover mismatch " ++ show (delta c,d)
      pure c

-- | @compose f g@ provides @f . g@
instance Composable Change where
  compose = flip change

-- | build a strictly more general function that produces the same answer on all accepted inputs.
--
-- generalize is idempotent
generalize :: Change -> Change
generalize (Change xs d) = foldMap go xs <> cpy d where
  go (Edit n f t) | k <- min f t = fromEdit (Edit (n + k) (dropDelta k f) (dropDelta k t))
