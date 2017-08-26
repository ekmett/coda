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
-----------------------------------------------------------------------------

module Coda.Server.Protocol.Test
  ( protocolTests
  ) where

import Coda.Server.Protocol.Parser
import Coda.Server.Protocol.Builder
import Coda.Server.Protocol.Base
import Coda.Util.Instances ()
import Data.Aeson (ToJSON, FromJSON, Value(..))
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
import Data.Tagged
import Data.Void
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Providers as Tasty

goldenFile :: TestName -> FilePath
goldenFile name = "test" </> "tasty" </> "protocol" </> name <.> "golden"

data ParseTest = ParseTest String (Lazy.ByteString -> Tasty.Result)

instance IsTest ParseTest where
  run _opts (ParseTest name process) _progress = process <$> Lazy.readFile (goldenFile name)
  testOptions = Tagged []

-- perform a golden file test and round
golden :: (ToJSON a, FromJSON a, Eq a) => TestName -> a -> TestTree
golden name content 
  = testGroup name
  [ goldenVsString "builder" (goldenFile name) $ pure $ toLazyByteString $ buildRpc content
  , singleTest "parser" $ ParseTest name $ \lbs -> case runParser eitherDecodeRpc lbs of
      Err -> testFailed "bad content wrapper"
      OK esr rest 
        | not (Lazy.null rest) -> testFailed "leftover content"
        | otherwise -> case esr of
          Left err -> testFailed $ "json parse failure: " ++ err
          Right content'
            | content' /= content  -> testFailed "content mismatch"
            | otherwise            -> testPassed ""
  ]

protocolTests :: TestTree
protocolTests = testGroup "protocol"
  [ golden "request"      (Request (Just (IntId 1)) "request" Null :: Request Value)
  , golden "notification" (Notification "notification" [1,2] :: Request [Int])
  , golden "response"     (Response (Just "id") 2 Nothing :: Response (Maybe Void) Int)
  ]
