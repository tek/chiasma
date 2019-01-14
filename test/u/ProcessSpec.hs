{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module ProcessSpec(
  htf_thisModulesTests,
) where

import Conduit (ConduitT, Flush(..), Void, runConduit, yieldMany, sinkList, mapC, (.|))
import qualified Data.Conduit.Combinators as Conduit (take, concatMap)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (packChars, unpackChars)
import Data.Conduit.Process.Typed (Process, setStdin, setStdout, proc, withProcess, getStdin, getStdout, createSource)
import Data.List.Split (splitOn)
import Test.Framework
import Chiasma.Data.Conduit (createSinkFlush)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Native.Process (socketArg)
import Chiasma.Test.Tmux (tmuxSpec)

run :: Process (ConduitT (Flush ByteString) Void IO ()) (ConduitT () ByteString IO ()) () -> IO [String]
run prc = do
  let stdin = mapC (fmap packChars) .| getStdin prc
  let stdout = getStdout prc
  runConduit $ yieldMany [Chunk "list-panes\n"] .| stdin
  runConduit $ yieldMany [Flush] .| stdin
  runConduit $ stdout .| Conduit.take 2 .| mapC unpackChars .| Conduit.concatMap (splitOn "\n") .| sinkList

test_process :: IO ()
test_process =
  tmuxSpec $ \(TmuxNative sock) -> do
    let procConf = setStdin createSinkFlush $ setStdout createSource $ proc "tmux" $ socketArg sock ++ ["-C", "attach"]
    out <- withProcess procConf run
    assertBool (length out > 3)
