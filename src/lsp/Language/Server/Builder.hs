{-# language CPP #-}
{-# language OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2017-2018
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- JSON-RPC 2.0 message serialization
--
-----------------------------------------------------------------------------

module Language.Server.Builder
  ( buildMessage
  , buildEncoding
  , hPutMessage
  , hPutEncoding
  , putMessage
  , putEncoding
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
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

-- | Write a JSON-RPC 2.0 message to a given file handle from an Encoding
hPutEncoding :: MonadIO m => Handle -> Encoding -> m ()
hPutEncoding h a = liftIO $ do
  hPutBuilder h $ buildEncoding a
  hFlush h

-- | Write a JSON-RPC 2.0 message to a given file handle
hPutMessage :: (MonadIO m, ToJSON a) => Handle -> a -> m ()
hPutMessage h a = hPutEncoding h (toEncoding a)

-- | Write a JSON-RPC 2.0 message to stdout
putMessage :: (MonadIO m, ToJSON a) => a -> m ()
putMessage = putEncoding . toEncoding

-- | Write a JSON-RPC 2.0 message to stdout from an Encoding
putEncoding :: MonadIO m => Encoding -> m ()
putEncoding = hPutEncoding stdout
