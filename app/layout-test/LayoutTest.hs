{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LayoutTest where

import Control.Monad (replicateM, when, unless, forM)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Text

import Control.Monad.State (MonadState, evalStateT, get, put)
import Control.Monad.Except (MonadError, runExcept, throwError)

import Control.Lens hiding (elements)

import Test.Tasty
import Test.Tasty.QuickCheck

import Syntax.Token
import Syntax.Dyck
import Syntax.Rope
import Syntax.Prefix
import Relative.Cat hiding (null)
import Relative.Delta
import Rev
import qualified Syntax.Lexer as Lex
import Syntax.Layout

linesToLayouts :: Delta -> [Text] -> (Delta, [Layout])
linesToLayouts d0 ts =
  let
    f (d, ls) t =
      let dt = Delta . Text.lengthWord16 $ t
      in
        ( d <> dt
        , ls <> pure (dyckLayout dt (fromText t) (Lex.lex t))
        )
  in
    foldl' f (d0, mempty) ts

textToLayouts :: Text -> [Layout]
textToLayouts t =
  let
    ts' = Text.lines t
    ts = case ts' of
      [] -> []
      _ -> fmap (<> "\n") (init ts') ++ [last ts']
    f :: Int -> Layout
    f i = (\(x, y) -> g x y) $ splitAt i ts
    g :: [Text] -> [Text] -> Layout
    g x y =
      let
        (d, ls1) = linesToLayouts 0 x
        (_, ls2) = linesToLayouts d y
        f1 = fold ls1
        f2 = fold ls2
      in
        f1 <> f2
  in
    fmap f [0..length ts - 1]

allEq :: Eq a => [a] -> Bool
allEq xs =
  and $ zipWith (==) xs (tail xs)

newtype Layouts = Layouts [Layout]
  deriving (Eq)

showLayouts :: Layouts -> [Text]
showLayouts (Layouts ls) = foldMap (("" :) . showLayout) ls

showLayout :: Layout -> [Text]
showLayout (E d) =
  ["E " <> Text.pack (show d)]
showLayout (S d r) =
  ["S " <> Text.pack (show d)] ++
  fmap ("  " <>) (showRun r)
showLayout (V d l m r) =
  ["V " <> Text.pack (show d)] ++
  showCat "    " l ++
  fmap ("  " <>) (showRun m) ++
  showRevCat "    " r

showDycks :: Cat Dyck -> [Text]
showDycks ds = case preview _Cons ds of
  Nothing -> []
  Just (r, rs) -> [Text.pack (show r)] ++ showDycks rs

showRun :: Run -> [Text]
showRun (Run p ds ts es pr) =
  ["Run"] ++
  ["  " <> Text.pack (show p)] ++
  fmap ("  " <>) (showDycks ds) ++
  ["  " <> Text.pack (show ts)] ++
  ["  " <> Text.pack (show es)] ++
  ["  " <> Text.pack (show pr)]

showCat :: Text -> Cat Run -> [Text]
showCat pad cs = case preview _Cons cs of
  Nothing -> []
  Just (r, rs) -> fmap (pad <>) (showRun r) ++ showCat pad rs

showRevCat :: Text -> Rev Cat Run -> [Text]
showRevCat pad (Rev cs) = showCat pad (revCat cs)

instance Show Layouts where
  show = Text.unpack . Text.unlines .  showLayouts

data DeltaError =
    DeltaOutOfOrder Delta Delta
  | BadRange Delta Delta Delta
  | DyckEmpty Delta
  | DyckMismatch [Delta] [Delta]
  | DyckErrorMismatch [Delta] [Delta]
  deriving (Eq, Ord, Show)

checkLayouts :: [Layout] -> Either DeltaError ()
checkLayouts =
  runExcept . flip evalStateT (Delta 0) . traverse_ (\l -> put (Delta 0) >> checkLayout l)

checkLayout :: (MonadState Delta m, MonadError DeltaError m) => Layout -> m ()
checkLayout (E d) = do
  checkDelta d
  put d
checkLayout (S _ r) = do
  checkRun r
checkLayout (V _ l m r) = do
  checkCatRun l
  checkRun m
  checkRevCatRun r

checkDelta :: (MonadState Delta m, MonadError DeltaError m) => Delta -> m ()
checkDelta d = do
  d' <- get
  when (d < d') $
    throwError $ DeltaOutOfOrder d' d

checkRun :: (MonadState Delta m, MonadError DeltaError m) => Run -> m ()
checkRun (Run _ ds ts es _) = do
  d <- get
  dd <- checkCatDyck ds
  dt <- checkDyck ts
  when (dd /= dt) $
    throwError $ DyckMismatch dd dt
  when (null dd) $ do
    throwError $ DyckEmpty d
  let m = maximum dd
  checkErrors d m es
  put m
  pure ()

checkCatRun :: (MonadState Delta m, MonadError DeltaError m) => Cat Run -> m ()
checkCatRun cr = case preview _Cons cr of
  Nothing -> pure ()
  Just (r, rs) -> do
    checkRun r
    checkCatRun rs

checkRevCatRun :: (MonadState Delta m, MonadError DeltaError m) => Rev Cat Run -> m ()
checkRevCatRun (Rev r) = checkCatRun (revCat r)

checkDyck :: (MonadState Delta m, MonadError DeltaError m) => Dyck -> m [Delta]
checkDyck (Dyck _ ts1 _ ts2 _ _) = do
  let ts = catTokenDeltas ts1 <> catTokenDeltas ts2
  old <- get
  _ <- forM ts $ \d -> do
    checkDelta d
    put d
  put old
  pure ts

checkCatDyck :: (MonadState Delta m, MonadError DeltaError m) => Cat Dyck -> m [Delta]
checkCatDyck cs = case preview _Cons cs of
  Nothing -> pure []
  Just (d, ds) -> do
    a <- checkDyck d
    b <- checkCatDyck ds
    pure $ a ++ b

checkErrors :: (MonadError DeltaError m) => Delta -> Delta -> Cat LayoutMismatch -> m ()
checkErrors d1 d2 cs = case preview _Cons cs of
  Nothing -> pure ()
  Just (e, es) -> do
    checkError d1 d2 e
    checkErrors d1 d2 es

checkError :: (MonadError DeltaError m) => Delta -> Delta -> LayoutMismatch -> m ()
checkError d1 d2 (LayoutMismatch d _ _) = do
  unless (d1 <= d && d < d2) $
    throwError $ BadRange d1 d d2

tokenDeltas :: Token -> [Delta]
tokenDeltas (Token d _) = [d]
tokenDeltas (TokenName d _) = [d]
tokenDeltas (TokenKeyword d _) = [d]
tokenDeltas (TokenInteger d _) = [d]
tokenDeltas (TokenDouble d _) = [d]
tokenDeltas (TokenString d _) = [d]
tokenDeltas (TokenChar d _) = [d]
tokenDeltas (TokenNested _ ds) = catTokenDeltas ds
tokenDeltas (TokenMismatch  _ _ ds) = catTokenDeltas ds
tokenDeltas (TokenUnmatchedOpening _) = []
tokenDeltas (TokenUnmatchedClosing _) = []
tokenDeltas (TokenLexicalError d _) = [d]

catTokenDeltas :: Cat Token -> [Delta]
catTokenDeltas cs = case preview _Cons cs of
  Nothing -> []
  Just (t, ts) -> tokenDeltas t ++ catTokenDeltas ts

newtype Indent = Indent { unIndent :: Int }
  deriving (Eq, Ord, Show)

instance Arbitrary Indent where
  arbitrary = Indent <$> choose (1, 5)
  shrink = fmap Indent . shrink . unIndent

data IndentedLine = IndentedLine { _indent :: Text, _line :: Text}
  deriving (Eq, Ord, Show)

makeLenses ''IndentedLine

indentedLineToText :: IndentedLine -> Text
indentedLineToText (IndentedLine i l) = i <> l

instance Arbitrary IndentedLine where
  arbitrary = do
    Indent i <- arbitrary
    t <- elements ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
    pure $ IndentedLine (Text.pack (replicate (2 * i) ' ')) t

newtype ModelLines = ModelLines { modelLines :: [IndentedLine] }
  deriving (Eq, Ord, Semigroup, Monoid)

modelLinesToText :: ModelLines -> Text
modelLinesToText (ModelLines ts) =
  Text.unlines . fmap indentedLineToText $ ts

instance Show ModelLines where
  show = Text.unpack . modelLinesToText

instance Arbitrary ModelLines where
  arbitrary = do
    n <- choose (0, 10)
    xs <- replicateM n arbitrary
    pure $ ModelLines xs
  shrink (ModelLines xs) =
    ModelLines <$> shrinkList (const []) xs

newtype ModelLinesWithErrors = ModelLinesWithErrors ModelLines
  deriving (Eq, Ord, Semigroup, Monoid)

modelLinesWithErrorsToText :: ModelLinesWithErrors -> Text
modelLinesWithErrorsToText (ModelLinesWithErrors ml) = modelLinesToText ml

instance Show ModelLinesWithErrors where
  show = Text.unpack . modelLinesWithErrorsToText

hasTab :: ModelLines -> Bool
hasTab (ModelLines ls) = any (Text.isInfixOf "\t" . _indent) ls

addTab :: ModelLines -> Gen ModelLines
addTab (ModelLines ls) = do
  let l1 = length ls
  if l1 == 0
  then pure $ ModelLines []
  else do
    n <- choose (0, l1 - 1)
    let
      l = ls !! n
      l2 = Text.length (_indent l)
    m <- choose (0, l2 - 1)
    let ls' = ls & ix n . indent . ix m .~ '\t'
    pure $ ModelLines ls'

addTabs :: ModelLines -> Gen ModelLines
addTabs ml@(ModelLines ls) = do
  x <- choose (1 :: Int, min 3 (length ls) `div` 3)
  let
    go 0 l = pure l
    go n l = do
      l' <- addTab l
      go (pred n) l'
  go x ml

instance Arbitrary ModelLinesWithErrors where
  arbitrary = arbitrary >>= fmap ModelLinesWithErrors . addTabs
  shrink (ModelLinesWithErrors ml) =
    ModelLinesWithErrors <$> filter hasTab (shrink ml)

data ModelLinesWithDo =
    MLWDLines ModelLines
  | MLWDDo Indent ModelLinesWithDo
  | MLWDTwo ModelLinesWithDo ModelLinesWithDo
  | MLWDThree ModelLinesWithDo ModelLinesWithDo ModelLinesWithDo
  deriving (Eq, Ord)

hasDo :: ModelLinesWithDo -> Bool
hasDo (MLWDLines _) = False
hasDo (MLWDDo _ _) = True
hasDo (MLWDTwo m1 m2) = hasDo m1 || hasDo m2
hasDo (MLWDThree m1 m2 m3) = hasDo m1 || hasDo m2 || hasDo m3

modelLinesWithDoToModelLines :: ModelLinesWithDo -> ModelLines
modelLinesWithDoToModelLines (MLWDLines ml) =
  ml
modelLinesWithDoToModelLines (MLWDDo (Indent i) mlwd) =
  let
    ModelLines ml = modelLinesWithDoToModelLines mlwd
    it = Text.pack (replicate i ' ')
  in
    ModelLines . (IndentedLine it "do" :) . fmap (\(IndentedLine j l) -> IndentedLine (it <> j) l) $ ml
modelLinesWithDoToModelLines (MLWDTwo mlwd1 mlwd2) =
  modelLinesWithDoToModelLines mlwd1 <>
  modelLinesWithDoToModelLines mlwd2
modelLinesWithDoToModelLines (MLWDThree mlwd1 mlwd2 mlwd3) =
  modelLinesWithDoToModelLines mlwd1 <>
  modelLinesWithDoToModelLines mlwd2 <>
  modelLinesWithDoToModelLines mlwd3

modelLinesWithDoToText :: ModelLinesWithDo -> Text
modelLinesWithDoToText = modelLinesToText . modelLinesWithDoToModelLines

instance Show ModelLinesWithDo where
  show = Text.unpack . modelLinesWithDoToText

genMLWD :: Int -> Gen ModelLinesWithDo
genMLWD s =
  let
      genLines = MLWDLines <$> arbitrary
      genDo = MLWDDo <$> arbitrary <*> genMLWD (s - 1)
      s2 = s `div` 2
      genTwo = MLWDTwo <$> genMLWD s2 <*> genMLWD s2
      s3 = s `div` 3
      genThree = MLWDThree <$> genMLWD s3 <*> genMLWD s3 <*> genMLWD s3
  in
    if s <= 1 then genLines else oneof [genLines, genDo, genTwo, genThree]

instance Arbitrary ModelLinesWithDo where
  arbitrary =
    suchThat (sized genMLWD) hasDo
  shrink (MLWDLines ml) =
    MLWDLines <$> shrink ml
  shrink (MLWDDo i ml) =
    if hasDo ml then [ml] else [] ++ (MLWDDo i <$> shrink ml)
  shrink (MLWDTwo m1 m2) =
    if hasDo m1 then [m1] else [] ++
    if hasDo m2 then [m2] else [] ++
    [MLWDTwo n1 n2 | (n1, n2) <- shrink (m1, m2)]
  shrink (MLWDThree m1 m2 m3) =
    if hasDo m1 then [m1] else [] ++
    if hasDo m2 then [m2] else [] ++
    if hasDo m3 then [m3] else [] ++
    [MLWDThree n1 n2 n3 | (n1, n2, n3) <- shrink (m1, m2, m3)]

newtype ModelLinesWithDoAndErrors = ModelLinesWithDoAndErrors ModelLinesWithDo
  deriving (Eq, Ord)

modelLinesWithDoAndErrorsToText :: ModelLinesWithDoAndErrors -> Text
modelLinesWithDoAndErrorsToText (ModelLinesWithDoAndErrors mlwd) = modelLinesWithDoToText mlwd

instance Show ModelLinesWithDoAndErrors where
  show = Text.unpack . modelLinesWithDoAndErrorsToText

hasTabWd :: ModelLinesWithDo -> Bool
hasTabWd (MLWDLines ml) =
  hasTab ml
hasTabWd (MLWDDo _ mlwd) =
  hasTabWd mlwd
hasTabWd (MLWDTwo m1 m2) =
  hasTabWd m1 || hasTabWd m2
hasTabWd (MLWDThree m1 m2 m3) =
  hasTabWd m1 || hasTabWd m2 || hasTabWd m3

addTabWd :: ModelLinesWithDo -> Gen ModelLinesWithDo
addTabWd (MLWDLines ml) =
  MLWDLines <$> addTabs ml
addTabWd (MLWDDo i mlwd) =
  MLWDDo i <$> addTabWd mlwd
addTabWd (MLWDTwo m1 m2) =
  oneof [ MLWDTwo <$> addTabWd m1 <*> pure m2
        , MLWDTwo <$> pure m1 <*> addTabWd m2
        ]
addTabWd (MLWDThree m1 m2 m3) =
  oneof [ MLWDThree <$> addTabWd m1 <*> pure m2 <*> pure m3
        , MLWDThree <$> pure m1 <*> addTabWd m2 <*> pure m3
        , MLWDThree <$> pure m1 <*> pure m2 <*> addTabWd m3
        ]

instance Arbitrary ModelLinesWithDoAndErrors where
  arbitrary = arbitrary >>= fmap ModelLinesWithDoAndErrors . addTabWd
  shrink (ModelLinesWithDoAndErrors mlwd) =
    ModelLinesWithDoAndErrors <$> filter hasTabWd (shrink mlwd)

checkTextMerging :: Text -> Property
checkTextMerging x =
  let
    ls = textToLayouts x
  in
    case ls of
      (l@(V _ _ (Run p _ ts _ pr) (Rev r)) : _) ->
        if boring ts
        then
          case preview _Cons (revCat r) of
          Just (Run p' _ _ _ _, _) ->
            case (joinAndCompare p p', joinAndCompare pr p') of
              (Right LT, Right _) -> counterexample (show (Layouts (pure l))) (True === False)
              _ -> True === True
          Nothing -> True === True
        else
          True === True
      _ -> True === True

testAllEq :: Text -> Property
testAllEq x =
  let
    ls = textToLayouts x
  in
    counterexample (show (Layouts ls)) ((=== True) . allEq $ ls)

testDeltas :: Text -> Property
testDeltas x =
  let
    ls = textToLayouts x
  in
    counterexample (show (Layouts ls)) ((=== Right ()) . checkLayouts $ ls)

test_layout :: TestTree
test_layout = testGroup "layout"
  [
    testProperty "all eq (no do, no errors)" $ testAllEq . modelLinesToText
  , testProperty "merging (no do, no errors)" $ checkTextMerging . modelLinesToText
  , testProperty "deltas (no do, no errors)" $ testDeltas . modelLinesToText
  , testProperty "all eq (with do, no errors)" $ testAllEq . modelLinesWithDoToText
  , testProperty "merging (with do, no errors)" $ checkTextMerging . modelLinesWithDoToText
  , testProperty "deltas (with do, no errors)" $ testDeltas . modelLinesWithDoToText
  , testProperty "all eq (no do, with errors)" $ testAllEq . modelLinesWithErrorsToText
  , testProperty "merging (no do, with errors)" $ checkTextMerging . modelLinesWithErrorsToText
  , testProperty "deltas (no do, with errors)" $ testDeltas . modelLinesWithErrorsToText
  , testProperty "all eq (with do, with errors)" $ testAllEq . modelLinesWithDoAndErrorsToText
  , testProperty "merging (with do, with errors)" $ checkTextMerging . modelLinesWithDoAndErrorsToText
  , testProperty "deltas (with do, with errors)" $ testDeltas . modelLinesWithDoAndErrorsToText
  ]

