module Chiasma.Test.Util where

import Hedgehog (TestT, withTests, property, test)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

type UnitTest = TestT IO ()

unitTest ::
  TestName ->
  UnitTest ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test
