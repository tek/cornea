module Main where

import DeepErrorSpec (test_hoist)
import DeepReaderSpec (test_reader)
import DeepStateSpec (test_lens)
import Hedgehog (TestT, property, test, withTests)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "throw a nested error" test_hoist,
    unitTest "read a nested value" test_reader,
    unitTest "access nested state" test_lens
  ]

main :: IO ()
main =
  defaultMain tests
