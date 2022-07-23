module Chiasma.Test.Util where

import Hedgehog (property, test, withTests)
import Polysemy.Test (UnitTest)
import qualified System.Environment as Environment
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

checkEnv :: UnitTest -> UnitTest
checkEnv t =
  maybe unit (const t) =<< liftIO (Environment.lookupEnv "DISPLAY")

integrationTest ::
  TestName ->
  UnitTest ->
  TestTree
integrationTest desc =
  testProperty desc . withTests 1 . property . test . checkEnv
