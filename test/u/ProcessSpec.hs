{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ProcessSpec (htf_thisModulesTests) where

import Conduit (ConduitT, Flush(..), Void, mapC, runConduit, sinkList, yieldMany, (.|))
import Data.ByteString.Internal (packChars)
import qualified Data.Conduit.Combinators as Conduit (concatMap, take)
import Data.Conduit.Process.Typed (Process, createSource, getStdin, getStdout, proc, setStdin, setStdout, withProcess)
import qualified Data.Text as Text (splitOn)
import Test.Framework

import Chiasma.Data.Conduit (createSinkFlush)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Native.Process (socketArg)
import Chiasma.Test.Tmux (tmuxSpec)

run :: Process (ConduitT (Flush ByteString) Void IO ()) (ConduitT () ByteString IO ()) () -> IO [Text]
run prc = do
  let stdin' = mapC (fmap packChars) .| getStdin prc
  let stdout' = getStdout prc
  runConduit $ yieldMany [Chunk "list-panes\n"] .| stdin'
  runConduit $ yieldMany [Flush] .| stdin'
  runConduit $ stdout' .| Conduit.take 2 .| mapC decodeUtf8 .| Conduit.concatMap (Text.splitOn "\n") .| sinkList

test_process :: IO ()
test_process =
  tmuxSpec $ \(TmuxNative sock) -> do
    out <- withProcess (pc (args sock)) run
    assertBool (length out > 3)
    where
      pc =
        setStdin createSinkFlush . setStdout createSource . proc "tmux"
      args sock =
        toString <$> socketArg sock ++ ["-C", "attach"]
