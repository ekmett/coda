{-# language BangPatterns #-}
module Combinators where

import qualified Control.Applicative as A
import Control.Applicative (Alternative((<|>)))
import Control.Monad
import Data.List.NonEmpty

import Parser

-- | @'between' open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- > braces = between (symbol "{") (symbol "}")

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close
{-# INLINEABLE between #-}

-- | @'choice' ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding parser.
--
-- > choice = asum

-- | Combine two alternatives.
--
-- > eitherP a b = (Left <$> a) <|> (Right <$> b)

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINEABLE eitherP #-}

-- | @'count' n p@ parses @n@ occurrences of @p@. If @n@ is smaller or equal
-- to zero, the parser equals to @'pure' []@. Returns a list of @n@
-- values.
--
-- See also: 'skipCount', 'count''.

count :: Int -> Parser a -> Parser [a]
count n' p = fmap ($ []) (go id n')
  where
    go f !n =
      if n <= 0
        then pure f
        else do
          x <- p
          go (f . (x:)) (n - 1)
{-# INLINEABLE count #-}

-- | @'count'' m n p@ parses from @m@ to @n@ occurrences of @p@. If @n@ is
-- not positive or @m > n@, the parser equals to @'pure' []@. Returns a
-- list of parsed values.
--
-- Please note that @m@ /may/ be negative, in this case effect is the same
-- as if it were equal to zero.
--
-- See also: 'skipCount', 'count'.

count' :: Int -> Int -> Parser a -> Parser [a]
count' m' n' p =
  if n' > 0 && n' >= m'
    then fmap ($ []) (gom id m')
    else pure []
  where
    gom f !m =
      if m > 0
        then do
          x <- p
          gom (f . (x:)) (m - 1)
        else god f (if m' <= 0 then n' else n' - m')
    god f !d =
      if d > 0
        then do
          r <- A.optional p
          case r of
            Nothing -> pure f
            Just  x -> god (f . (x:)) (d - 1)
        else pure f
{-# INLINEABLE count' #-}

-- | @'endBy' p sep@ parses /zero/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a list of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semicolon

endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p >>= \x -> x <$ sep)
{-# INLINEABLE endBy #-}

-- | @'endBy1' p sep@ parses /one/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a list of values returned by @p@.

endBy1 :: Parser a -> Parser  sep -> Parser (NonEmpty a)
endBy1 p sep = some (p >>= \x -> x <$ sep)
{-# INLINEABLE endBy1 #-}

-- | @'many' p@ applies the parser @p@ /zero/ or more times and pures a
-- list of the values returned by @p@.
--
-- > identifier = (:) <$> letter <*> many (alphaNumChar <|> char '_')

many :: Parser a -> Parser [a]
many p = fmap ($ []) (go id)
  where
    go f = do
      r <- A.optional p
      case r of
        Nothing -> pure f
        Just  x -> go (f . (x:))
{-# INLINEABLE many #-}

-- | @'manyTill' p end@ applies parser @p@ /zero/ or more times until parser
-- @end@ succeeds. Returns the list of values returned by @p@.
--
-- See also: 'skipMany', 'skipManyTill'.

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = fmap ($ []) (go id)
  where
    go f = do
      done <- (True <$ end) <|> pure False
      if done
        then pure f
        else do
          x <- p
          go (f . (x:))
{-# INLINEABLE manyTill #-}

-- | @'some' p@ applies the parser @p@ /one/ or more times and pures a
-- list of the values returned by @p@.
--
-- > word = some letter

some :: Parser a -> Parser (NonEmpty a)
some p = liftM2 (:|) p (many p)
{-# INLINEABLE some #-}

-- | @'someTill' p end@ works similarly to @'manyTill' p end@, but @p@
-- should succeed at least once.
--
-- See also: 'skipSome', 'skipSomeTill'.

someTill :: Parser a -> Parser end -> Parser (NonEmpty a)
someTill p end = liftM2 (:|) p (manyTill p end)
{-# INLINEABLE someTill #-}

-- | @'sepBy' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = do
  r <- A.optional p
  case r of
    Nothing -> pure []
    Just  x -> fmap (x:) (many (sep >> p))
{-# INLINEABLE sepBy #-}

-- | @'sepBy1' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.

sepBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1 p sep = do
  x <- p
  fmap (x:|) (many (sep >> p))
{-# INLINEABLE sepBy1 #-}

-- | @'sepEndBy' p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy p sep = fmap ($ []) (go id)
  where
    go f = do
      r <- A.optional p
      case r of
        Nothing -> pure f
        Just  x -> do
          more <- True <$ sep <|> pure False
          if more
            then go (f . (x:))
            else pure (f . (x:))
{-# INLINEABLE sepEndBy #-}

-- | @'sepEndBy1' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepEndBy1 p sep = do
  x <- p
  more <- True <$ sep <|> pure False
  if more
    then fmap (x:|) (sepEndBy p sep)
    else pure (pure x)
{-# INLINEABLE sepEndBy1 #-}

-- | @'skipMany' p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- See also: 'manyTill', 'skipManyTill'.

skipMany :: Parser a -> Parser ()
skipMany p = go where
  go = do
   more <- True <$ p <|> pure False
   when more go
{-# INLINEABLE skipMany #-}

-- | @'skipSome' p@ applies the parser @p@ /one/ or more times, skipping its
-- result.
--
-- See also: 'someTill', 'skipSomeTill'.

skipSome :: Parser a -> Parser ()
skipSome p = p >> skipMany p
{-# INLINEABLE skipSome #-}

-- | @'skipCount' n p@ parses @n@ occurrences of @p@, skipping its result.
-- If @n@ is smaller or equal to zero, the parser equals to @'pure' []@.
-- Returns a list of @n@ values.
--
-- See also: 'count', 'count''.

skipCount :: Int -> Parser a -> Parser ()
skipCount n' p = go n' where
  go !n = unless (n <= 0) $ p >> go (n - 1)
{-# INLINEABLE skipCount #-}

-- | @'skipManyTill' p end@ applies the parser @p@ /zero/ or more times
-- skipping results until parser @end@ succeeds. Result parsed by @end@ is
-- then returned.
--
-- See also: 'manyTill', 'skipMany'.

skipManyTill :: Parser a -> Parser end -> Parser end
skipManyTill p end = go where
  go = do
    r <- A.optional end
    case r of
      Nothing -> p >> go
      Just  x -> pure x
{-# INLINEABLE skipManyTill #-}

-- | @'skipSomeTill' p end@ applies the parser @p@ /one/ or more times
-- skipping results until parser @end@ succeeds. Result parsed by @end@ is
-- then returned.
--
-- See also: 'someTill', 'skipSome'.

skipSomeTill :: Parser a -> Parser end -> Parser end
skipSomeTill p end = p >> skipManyTill p end
{-# INLINEABLE skipSomeTill #-}

----------------------------------------------------------------------------
-- Compat helpers (for older GHCs)

option :: a -> Parser a -> Parser a
option x p = p `mplus` pure x
{-# INLINEABLE option #-}
