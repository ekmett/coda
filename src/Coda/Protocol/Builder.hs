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

module Coda.Protocol.Builder
  ( buildRpc
  , hPutRpc
  , putRpc
#ifdef TEST_TASTY
  , testProtocolBuilder
#endif
  ) where

import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import System.IO

#ifdef TEST_TASTY
import Coda.Instances ()
import Coda.Protocol.Base
import Data.Void
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
#endif

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

#ifdef TEST_TASTY
testProtocol :: ToJSON a => TestName -> a -> TestTree
testProtocol name content 
  = goldenVsString name ("test" </> "protocol" </> name <.> "golden")
  $ pure
  $ toLazyByteString $ buildRpc content

testProtocolBuilder :: TestTree
testProtocolBuilder = testGroup "protocol"
  [ testProtocol "request"      (Request (Just (IntId 1)) "request" Nothing :: Request Void)
  , testProtocol "notification" (Notification "notification" (Just [1,2]) :: Request [Int])
  , testProtocol "response"     (Response (Just "id") (Just 2) Nothing :: Response Void Int)
  ]
#endif
