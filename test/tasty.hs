module Main where

import Coda.Protocol.Test (protocolTests)
import Test.Tasty
import Test.Tasty.HUnit

test :: TestTree
test = testCase "Test 1" $ (2::Int) @?= 2

main :: IO ()
main = defaultMain $ testGroup "tasty" 
  [ test
  , protocolTests
  ]
