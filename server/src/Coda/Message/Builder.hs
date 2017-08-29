{-# language CPP #-}
{-# language OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Edward Kmett
-- License     :  BSD2 (see the file LICENSE.md)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- JSON-RPC 2.0 message serialization
--
-----------------------------------------------------------------------------

module Coda.Message.Builder
  ( buildMessage
  , buildEncoding
  , hPutMessage
  , hPutEncoding
  , putMessage
  , putEncoding
  ) where

import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import System.IO

-- | Serialize a JSON-RPC 2.0 message.
--
-- >>> toLazyByteString (buildMessage "hello")
-- "Content-Length: 7\r\n\r\n\"hello\""
buildMessage :: ToJSON a => a -> Builder
buildMessage = buildEncoding . toEncoding

-- | Serialize a JSON-RPC 2.0 message from an Encoding
buildEncoding :: Encoding -> Builder
buildEncoding a
    = byteString "Content-Length: " <> int64Dec (Lazy.length content) <> byteString "\r\n\r\n"
   <> lazyByteString content
  where content = toLazyByteString (fromEncoding a)

-- | Write a JSON-RPC 2.0 message to a given file handle
hPutMessage :: ToJSON a => Handle -> a -> IO ()
hPutMessage h = hPutEncoding h . toEncoding

-- | Write a JSON-RPC 2.0 message to a given file handle from an Encoding
hPutEncoding :: Handle -> Encoding -> IO ()
hPutEncoding h = hPutBuilder h . buildEncoding

-- | Write a JSON-RPC 2.0 message to stdout
putMessage :: ToJSON a => a -> IO ()
putMessage = putEncoding . toEncoding

-- | Write a JSON-RPC 2.0 message to stdout from an Encoding
putEncoding :: Encoding -> IO ()
putEncoding = hPutEncoding stdout
