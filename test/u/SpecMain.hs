{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} CodecSpec
import {-@ HTF_TESTS @-} IndividualProcessSpec
import {-@ HTF_TESTS @-} LensSpec
import {-@ HTF_TESTS @-} OutputParseSpec
import {-@ HTF_TESTS @-} ProcessSpec
import {-@ HTF_TESTS @-} RenderSpec
import {-@ HTF_TESTS @-} StreamParseSpec
import {-@ HTF_TESTS @-} TmuxStreamSpec

main :: IO ()
main = htfMain htf_importedTests
