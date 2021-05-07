module Chiasma.Test.ProcessTest where

import Chiasma.Data.Conduit (createSinkFlush)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Native.Process (socketArg)
import Conduit (ConduitT, Flush(..), mapC, runConduit, sinkList, yieldMany, (.|))
import Data.ByteString.Internal (packChars)
import qualified Data.Conduit.Combinators as Conduit (concatMap, take)
import Data.Conduit.Process.Typed (
  Process,
  createSource,
  getStdin,
  getStdout,
  proc,
  setStdin,
  setStdout,
  withProcessTerm,
  )
import qualified Data.Text as Text (splitOn)
import Hedgehog (assert)

import Chiasma.Test.Tmux (tmuxSpec)
import Chiasma.Test.Util (UnitTest)

run :: Process (ConduitT (Flush ByteString) Void IO ()) (ConduitT () ByteString IO ()) () -> IO [Text]
run prc = do
  let stdin' = mapC (fmap packChars) .| getStdin prc
  let stdout' = getStdout prc
  runConduit $ yieldMany ([Chunk "list-panes\n"] :: [Flush String]) .| stdin'
  runConduit $ yieldMany ([Flush] :: [Flush String]) .| stdin'
  runConduit $ stdout' .| Conduit.take 2 .| mapC decodeUtf8 .| Conduit.concatMap (Text.splitOn "\n") .| sinkList

test_process :: UnitTest
test_process = do
  out <- liftIO $ tmuxSpec $ \(TmuxNative sock) -> do
    withProcessTerm (pc (args sock)) run
  assert (length out > 3)
    where
      pc =
        setStdin createSinkFlush . setStdout createSource . proc "tmux"
      args sock =
        toString <$> socketArg sock ++ ["-C", "attach"]
