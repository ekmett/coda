{-# language OverloadedStrings #-}
module Coda.Rpc.Builder
  ( buildRpc
  , hPutRpc
  , putRpc
  ) where

import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import System.IO

buildRpc :: ToJSON a => a -> Builder
buildRpc a = byteString "Content-Length: " <> int64Dec (Lazy.length content) <> byteString "\r\n\r\n" <> lazyByteString content where
  content = toLazyByteString (fromEncoding (toEncoding a))

hPutRpc :: ToJSON a => Handle -> a -> IO () 
hPutRpc h a = hPutBuilder h (buildRpc a)

putRpc :: ToJSON a => a -> IO ()
putRpc = hPutRpc stdout
