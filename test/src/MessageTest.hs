{-# language LambdaCase #-}
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

module MessageTest
  ( test_message
  ) where

import Coda.Util.Instances ()
import Control.Lens ((<&>))
import Data.Aeson (ToJSON, FromJSON, Value(..), fromJSON, toJSON, Result(..))
import Data.ByteString.Builder
import Data.Tagged
import Language.Server.Parser
import Language.Server.Builder
import Language.Server.Base
import System.FilePath
import System.IO
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.Providers as Tasty

goldenFile :: TestName -> FilePath
goldenFile name = "test" </> "data" </> "message" </> name <.> "golden"

newtype ParseTest = ParseTest (IO Tasty.Result)
instance IsTest ParseTest where
  run _ (ParseTest r) _ = r
  testOptions = Tagged []

-- | perform a golden file test and round-trip test
golden :: (ToJSON a, FromJSON a, Show a, Eq a) => TestName -> a -> TestTree
golden name content
  = testGroup name
  [ goldenVsString "encoding" (goldenFile name) $
      pure $ toLazyByteString $ buildMessage content
  , singleTest "parser" $ ParseTest $
      withFile (goldenFile name) ReadMode $ \handle ->
        parse eitherDecodeMessage' handle >>= \case
          Left e         -> pure $ testFailed $ "bad JSON-RPC frame: " ++ e
          Right (Left e) -> pure $ testFailed $ "bad JSON message: " ++ e
          Right (Right content')
            | content' /= content -> pure $ testFailed "content mismatch"
            | otherwise -> hIsEOF handle <&> \fin -> if fin
              then testPassed ""
              else testFailed "leftover content"
  , testCase "value" $ fromJSON (toJSON content) @=? Success content
  ]

test_message :: TestTree
test_message = testGroup "message"
  [ golden "request" $ Request (Just (IntId 1)) "request" Nothing
  , golden "response" $ Response (Just "id") (Just (Number 2)) Nothing
  ]
