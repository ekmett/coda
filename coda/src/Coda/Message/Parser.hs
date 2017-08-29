{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD2 (see the file LICENSE.md)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- JSON-RPC 2.0 message parsing
--
-----------------------------------------------------------------------------

module Coda.Message.Parser 
  ( Parser(..)
  , ParseResult(..)
  , message
  , decodeMessage
  , decodeMessage'
  , eitherDecodeMessage
  , eitherDecodeMessage'
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Aeson
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as Lazy

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

-- | This parser consumes lazy bytestrings
newtype Parser a = Parser { runParser :: Lazy.ByteString -> ParseResult a }
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> OK a s

  Parser m <*> Parser n = Parser $ \s -> case m s of
    Err -> Err
    OK f s' -> case n s' of 
      Err -> Err
      OK a s'' -> OK (f a) s''

  Parser m *> Parser n = Parser $ \s -> case m s of
    Err -> Err
    OK _ s' -> n s'

  Parser m <* Parser n = Parser $ \s -> case m s of
    Err -> Err
    OK a s' -> case n s' of
      Err -> Err
      OK _ s'' -> OK a s''

instance Monad Parser where
  Parser m >>= k = Parser $ \s -> case m s of
    Err -> Err
    OK a s' -> runParser (k a) s'

  (>>) = (*>)

  fail _ = empty

instance Alternative Parser where
  empty = Parser $ const Err
  Parser m <|> Parser n = Parser $ \s -> case m s of 
    Err -> n s
    ok -> ok

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance MonadFail Parser where
  fail _ = empty

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral

-- | Parse one byte as an ASCII character
ascii :: Parser Char
ascii = Parser $ \s -> case Lazy.uncons s of
  Just (w8, s') -> OK (w2c w8) s'
  Nothing -> Err

-- | Parse exactly the string specified
string :: Lazy.ByteString -> Parser ()
string p = Parser $ \s -> case Lazy.stripPrefix p s of -- 0.10.8
  Nothing -> Err
  Just s' -> OK () s'

-- | Parse to the next carriage return
line :: Parser Lazy.ByteString
line = Parser $ \s -> case Lazy.break (== c2w '\r') s of
    (p, s') -> OK p s'
  
-- | Parse an integer
--
-- >>> runParser int64 "123what"
-- OK 123 "what"
int64 :: Parser Int64
int64 = Parser $ \s -> case Lazy.uncons s of
    Just (w8, s') 
      | w8 >= 48 && w8 <= 57 -> case acc (fromIntegral w8 - 48) s' of -- require at least one digit
        (i, s'') -> OK i s''
    _ -> Err
  where
  acc !i s = case Lazy.uncons s of
    Just (w8, s') 
      | w8 >= 48 && w8 <= 57 -> acc (i * 10 + fromIntegral (w8 - 48)) s'
    _ -> (i, s)

-- | Match a carriage return and line feed pair
crlf :: Parser ()
crlf = string "\r\n"

-- | Parse a JSON-RPC 2.0 content header
--
-- TODO: validate Content-Type
contentHeader :: Parser Int64
contentHeader = do
  string "Content-"
  ascii >>= \case
    'L' -> string "ength: " *> int64 <* (crlf *> rest)
    'T' -> string "ype: " *> line *> crlf *> contentHeader
    _ -> empty
 where
   rest = crlf -- end of header
      <|> string "Content-Type: " *> line *> crlf *> rest

-- | Consume @n@ bytes
bytes :: Int64 -> Parser Lazy.ByteString
bytes n = Parser $ \s -> case Lazy.splitAt n s of
  (p, s') | Lazy.length p == n -> OK p s'
          | otherwise -> Err

--------------------------------------------------------------------------------
-- * RPC Parsing
--------------------------------------------------------------------------------

-- | This parses a JSON-RPC 2.0 message
--
-- This stops before we get to actually decoding the JSON message.
message :: Parser Lazy.ByteString
message = contentHeader >>= bytes

-- | This decodes a JSON-RPC 2.0 message lazily
--
-- If the outer parser fails, then the message stream is unrecoverable. If decoding fails, we simply failed to read this message.
decodeMessage :: FromJSON a => Parser (Maybe a)
decodeMessage = decode <$> message

-- | This decodes a JSON-RPC 2.0 message eager
--
-- If the outer parser fails, then the message stream is unrecoverable. If decoding fails, we simply failed to read this message.
decodeMessage' :: FromJSON a => Parser (Maybe a)
decodeMessage' = decode' <$> message

-- | This decodes a JSON-RPC 2.0 message lazily with an error message on failure
eitherDecodeMessage :: FromJSON a => Parser (Either String a)
eitherDecodeMessage = eitherDecode <$> message

-- | This decodes a JSON-RPC 2.0 message eager with an error message on failure
eitherDecodeMessage' :: FromJSON a => Parser (Either String a)
eitherDecodeMessage' = eitherDecode' <$> message
