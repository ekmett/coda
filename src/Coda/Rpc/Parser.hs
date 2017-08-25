{-# language BangPatterns #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
module Coda.Rpc.Parser 
  ( Parser(..)
  , ParseResult(..)
  , rpc
  , decodeRpc
  , decodeRpc'
  , eitherDecodeRpc
  , eitherDecodeRpc'
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Aeson
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as Lazy

data ParseResult a
  = Err 
  | OK !a !Lazy.ByteString
  deriving (Show, Functor, Foldable, Traversable)

-- this parser consumes lazy bytestrings
data Parser a = Parser { runParser :: Lazy.ByteString -> ParseResult a }
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
  empty = Parser $ \_ -> Err
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

-- | Parse one byte
byte :: Parser Word8
byte = Parser $ \s -> case Lazy.uncons s of
  Just (w8, s) -> OK w8 s
  Nothing -> Err

-- | Parse exactly the string specified
string :: Lazy.ByteString -> Parser ()
string p = Parser $ \s -> case Lazy.stripPrefix p s of -- 0.10.8
  Nothing -> Err
  Just s' -> OK () s'

cr :: Word8
cr = c2w '\r'

-- | Parse to the next carriage return
line :: Parser Lazy.ByteString
line = Parser $ \s -> case Lazy.break (== cr) s of
    (p, s') -> OK p s'
  

-- | Parse an integer
int64 :: Parser Int64
int64 = Parser $ \s -> case Lazy.uncons s of
    Just (w8, s') 
      | w8 >= 48 && w8 <= 57 -> case acc (fromIntegral w8 - 48) s' of -- require at least one digit
        (i, s') -> OK i s'
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
content_header :: Parser Int64
content_header = do
  string "Content-"
  byte >>= \w -> case w2c w of
    'L' -> string "ength: " *> int64 <* (crlf *> rest)
    'T' -> string "ype: " *> line *> crlf *> content_header
    _ -> empty
 where
   rest = crlf -- end of header
      <|> string "Content-Type: " *> line *> crlf *> rest

-- | Consume @n@ bytes
bytes :: Int64 -> Parser Lazy.ByteString
bytes n = Parser $ \s -> case Lazy.splitAt n s of
  (p, s') | Lazy.length p == n -> OK p s'
          | otherwise -> Err

-- | This parses a JSON-RPC 2.0 message
rpc :: Parser Lazy.ByteString
rpc = content_header >>= bytes

-- | This decodes a JSON-RPC 2.0 message lazily
decodeRpc :: FromJSON a => Parser (Maybe a)
decodeRpc = decode <$> (content_header >>= bytes)

-- | This decodes a JSON-RPC 2.0 message eager
decodeRpc' :: FromJSON a => Parser (Maybe a)
decodeRpc' = decode' <$> (content_header >>= bytes)

-- | This decodes a JSON-RPC 2.0 message lazily with an error message on failure
eitherDecodeRpc :: FromJSON a => Parser (Either String a)
eitherDecodeRpc = eitherDecode <$> (content_header >>= bytes)

-- | This decodes a JSON-RPC 2.0 message eager with an error message on failure
eitherDecodeRpc' :: FromJSON a => Parser (Either String a)
eitherDecodeRpc' = eitherDecode' <$> (content_header >>= bytes)
