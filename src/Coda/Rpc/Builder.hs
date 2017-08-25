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

module Coda.Rpc.Builder
  ( buildRpc
  , hPutRpc
  , putRpc
  ) where

import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import System.IO

-- | Serialize a JSON-RPC 2.0 message.
--
-- >>> toLazyByteString (buildRpc "hello")
-- "Content-Length: 7\r\n\r\n\"hello\""
buildRpc :: ToJSON a => a -> Builder
buildRpc a = byteString "Content-Length: " <> int64Dec (Lazy.length content) <> byteString "\r\n\r\n" <> lazyByteString content where
  content = toLazyByteString (fromEncoding (toEncoding a))

-- | Write a JSON-RPC 2.0 message to a given file handle
hPutRpc :: ToJSON a => Handle -> a -> IO () 
hPutRpc h a = hPutBuilder h (buildRpc a)

-- | Write a JSON-RPC 2.0 message to stdout
putRpc :: ToJSON a => a -> IO ()
putRpc = hPutRpc stdout
