{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} BufferedSpec
import {-@ HTF_TESTS @-} OutputParseSpec
import {-@ HTF_TESTS @-} CodecSpec
import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = htfMain htf_importedTests
