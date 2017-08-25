{-# language OverloadedStrings #-}
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
buildRpc :: ToJSON a => a -> Builder
buildRpc a = byteString "Content-Length: " <> int64Dec (Lazy.length content) <> byteString "\r\n\r\n" <> lazyByteString content where
  content = toLazyByteString (fromEncoding (toEncoding a))

-- | Write a JSON-RPC 2.0 message to a given file handle
hPutRpc :: ToJSON a => Handle -> a -> IO () 
hPutRpc h a = hPutBuilder h (buildRpc a)

-- | Write a JSON-RPC 2.0 message to stdout
putRpc :: ToJSON a => a -> IO ()
putRpc = hPutRpc stdout
