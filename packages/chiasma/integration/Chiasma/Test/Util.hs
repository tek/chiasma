module Chiasma.Test.Util where

import Hedgehog (TestT, withTests, property, test)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)
import qualified System.Environment as Environment

type UnitTest = TestT IO ()

checkEnv :: UnitTest -> UnitTest
checkEnv t =
  maybe unit (const t) =<< liftIO (Environment.lookupEnv "DISPLAY")

integrationTest ::
  TestName ->
  UnitTest ->
  TestTree
integrationTest desc =
  testProperty desc . withTests 1 . property . test . checkEnv
