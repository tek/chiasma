module Chiasma.Test.Util where

import Hedgehog (property, test, withTests)
import Polysemy.Test (UnitTest)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

integrationTest ::
  TestName ->
  UnitTest ->
  TestTree
integrationTest desc =
  testProperty desc . withTests 1 . property . test
