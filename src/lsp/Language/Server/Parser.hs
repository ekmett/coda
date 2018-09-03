{-# language LambdaCase #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- JSON-RPC 2.0 message parsing
--
-----------------------------------------------------------------------------

module Language.Server.Parser 
  ( Parser(..)
  , ParseResult(..)
  , parse
  , parseMessage
  , decodeMessage
  , decodeMessage'
  , eitherDecodeMessage
  , eitherDecodeMessage'
  ) where

import Control.Monad
import Data.Aeson
import Data.Char
import Data.Data
import qualified Data.ByteString.Lazy as Lazy
import System.IO
import Control.Exception

-- $setup
-- >>> :set -XOverloadedStrings

--------------------------------------------------------------------------------
-- * Lazy ByteString Parsing
--------------------------------------------------------------------------------

-- | The result of parsing
data ParseResult a
  = Err 
  | OK !a !Lazy.ByteString
  deriving (Show, Functor, Foldable, Traversable)

-- | LL(1) Parser for streaming directly from file handles.
--
-- Assumes the input handle has been set to
--
-- @
-- 'hSetBuffering' handle 'NoBuffering'
-- 'hSetEncoding' handle 'char8'
-- @
newtype Parser a = Parser { runParser :: Handle -> IO a }
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \_ -> pure a
  (<*>) = ap
  (*>) = (>>)

instance Monad Parser where
  Parser m >>= f = Parser $ \h -> m h >>= \a -> runParser (f a) h
  fail s = Parser $ \_ -> throw $ ParseError s

parse :: Parser a -> Handle -> IO (Either String a)
parse p h = (Right <$> runParser p h) `catch` \(ParseError e) -> pure $ Left e

-- | parse errors for json-rpc frames are basically unrecoverable as there is no real framing
newtype ParseError = ParseError String
  deriving (Show, Data)

instance Exception ParseError

-- | Parse one byte as an ISO-8859-1 character
ascii :: Parser Char
ascii = Parser hGetChar

char :: Char -> Parser ()
char p = do
  q <- ascii
  unless (p == q) $ fail $ "expected " ++ show p

-- | Parse exactly the string specified
string :: Lazy.ByteString -> Parser ()
string p = Parser $ \h -> do 
  q <- Lazy.hGet h (fromIntegral $ Lazy.length p)
  unless (p == q) $ fail $ "expected " ++ show p

-- | Parse to the next carriage return and linefeed inclusively
anyField :: Parser ()
anyField = ascii >>= \case
  'r' -> char '\n'
  _ -> anyField
  
-- | Parse an integer followed by a carriage return linefeed
intField :: Parser Int
intField = do
  b <- ascii
  unless (isDigit b) $ fail "expected integer"
  go (digitToInt b)
 where
  go :: Int -> Parser Int
  go acc = ascii >>= \case
    '\r' -> acc <$ char '\n'
    d | isDigit d -> go (acc * 10 + digitToInt d)
      | otherwise -> fail "expected digit or '\\r'"

-- | Parse a JSON-RPC 2.0 content header
--
-- TODO: validate Content-Type
contentHeader :: Parser Int
contentHeader = do
  char 'C' -- give an error back one character in
  string "ontent-"
  ascii >>= \case
    'L' -> string "ength: " *> intField <* rest
    'T' -> string "ype: " *> anyField *> contentHeader
    _ -> fail "expected 'L' or 'T'"
 where
   rest = ascii >>= \case
     '\r' -> char '\n'
     'C'  -> string "ontent-Type: " *> anyField *> rest
     _ -> fail "expected '\\r' or 'C'"

-- | Consume @n@ bytes
bytes :: Int -> Parser Lazy.ByteString
bytes n = Parser $ \h -> do
  bs <- Lazy.hGet h n
  let m = fromIntegral (Lazy.length bs)
  unless (m == n) $ fail $ "expected " ++ show n ++ " bytes, but only received " ++ show m
  pure bs

--------------------------------------------------------------------------------
-- * RPC Parsing
--------------------------------------------------------------------------------

-- | This parses a JSON-RPC 2.0 message
--
-- This stops before we get to actually decoding the JSON message.
parseMessage :: Parser Lazy.ByteString
parseMessage = contentHeader >>= bytes

-- | This decodes a JSON-RPC 2.0 message lazily
--
-- If the outer parser fails, then the message stream is unrecoverable. If decoding fails, we simply failed to read this message.
decodeMessage :: FromJSON a => Parser (Maybe a)
decodeMessage = decode <$> parseMessage

-- | This decodes a JSON-RPC 2.0 message eager
--
-- If the outer parser fails, then the message stream is unrecoverable. If decoding fails, we simply failed to read this message.
decodeMessage' :: FromJSON a => Parser (Maybe a)
decodeMessage' = decode' <$> parseMessage

-- | This decodes a JSON-RPC 2.0 message lazily with an error message on failure
eitherDecodeMessage :: FromJSON a => Parser (Either String a)
eitherDecodeMessage = eitherDecode <$> parseMessage

-- | This decodes a JSON-RPC 2.0 message eager with an error message on failure
eitherDecodeMessage' :: FromJSON a => Parser (Either String a)
eitherDecodeMessage' = eitherDecode' <$> parseMessage
