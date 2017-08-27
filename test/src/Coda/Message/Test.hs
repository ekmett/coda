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

module Coda.Message.Test
  ( test_message
  ) where

import Coda.Message.Parser
import Coda.Message.Builder
import Coda.Message.Base
import Coda.Util.Instances ()
import Data.Aeson (ToJSON, FromJSON, Value(..))
import Data.ByteString.Builder
import Data.ByteString.Lazy as Lazy
import Data.Tagged
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Providers as Tasty

goldenFile :: TestName -> FilePath
goldenFile name = "test" </> "data" </> "message" </> name <.> "golden"

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

test_message :: TestTree
test_message = testGroup "message"
  [ golden "request"      (Request (IntId 1) "request" Null :: Request Id Value)
  , golden "notification" (Notification "notification" [1,2] :: Notification [Int])
  , golden "response"     (Response "id" 2 Nothing :: Response Value Id Int)
  ]
