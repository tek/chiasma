module Chiasma.Test.ProcessOutputTest where

import qualified Data.ByteString as ByteString
import Hedgehog ((===))
import Polysemy.Process.Effect.ProcessOutput (OutputPipe (Stdout), ProcessOutput (Chunk))

import qualified Chiasma.Data.TmuxEvent as TmuxEvent
import Chiasma.Data.TmuxNotification (TmuxNotification (..))
import Chiasma.Data.TmuxOutputBlock (TmuxOutputBlock (..))
import Chiasma.Interpreter.ProcessOutput (interpretProcessOutputTmuxEvent)
import Chiasma.Test.Util (UnitTest)

-- | Simulate a 'Chunk' call via the interpreter, returning parsed events.
runChunk ::
  ByteString ->
  ByteString ->
  IO ([Either Text TmuxEvent.TmuxEvent], ByteString)
runChunk buffer new =
  runM $
  interpretProcessOutputTmuxEvent @'Stdout do
    send (Chunk buffer new)

-- | Two notifications concatenated in a single chunk.
-- The bug causes only the first to be parsed, the second is lost.
test_multiNotificationChunk :: UnitTest
test_multiNotificationChunk = do
  let
    chunk = ByteString.intercalate "" [
      "%window-add @1\n",
      "%session-changed $0 main\n"
      ]
  (events, _leftover) <- liftIO (runChunk "" chunk)
  [Right (TmuxEvent.Notification (TmuxNotification "window-add" ["@1"])),
   Right (TmuxEvent.Notification (TmuxNotification "session-changed" ["$0", "main"]))]
    === events

-- | A response block followed by a notification in one chunk.
test_responseAndNotificationChunk :: UnitTest
test_responseAndNotificationChunk = do
  let
    chunk = ByteString.intercalate "" [
      "%begin 123\n",
      "some data\n",
      "%end 123\n",
      "%window-add @5\n"
      ]
  (events, _leftover) <- liftIO (runChunk "" chunk)
  [Right (TmuxEvent.Response (Success ["some data"])),
   Right (TmuxEvent.Notification (TmuxNotification "window-add" ["@5"]))]
    === events

-- | Leftovers from a previous chunk are passed as the buffer argument.
-- The bug causes the buffer to be discarded.
test_bufferContinuation :: UnitTest
test_bufferContinuation = do
  let
    buffer = "%window-add"
    new = " @3\n"
  (events, _leftover) <- liftIO (runChunk buffer new)
  [Right (TmuxEvent.Notification (TmuxNotification "window-add" ["@3"]))]
    === events
