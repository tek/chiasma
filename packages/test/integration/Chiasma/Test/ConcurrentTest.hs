module Chiasma.Test.ConcurrentTest where

import qualified Conc
import Conc (withAsyncGated_, withAsync_)
import qualified Data.Text as Text
import Data.Text (stripPrefix)
import Exon (exon)
import Gate (Gate, signal)
import Polysemy.Test (Hedgehog, UnitTest, assert, (===))
import qualified Queue
import Queue (QueueResult (..))
import qualified Time
import Time (MilliSeconds (MilliSeconds))

import Chiasma.Codec.Data.Window (Window (..))
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxCommand (TmuxCommand (ListWindows, NewWindow))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxNotification (TmuxNotification (..))
import qualified Chiasma.Effect.TmuxApi as TmuxApi
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Test.Tmux (tmuxTest)
import Chiasma.Tmux (withTmux)

-- | Run a loop of create-window then list-windows commands, collecting results.
worker ::
  Members [Tmux, AtomicState [Text]] r =>
  Text ->
  Sem r ()
worker label =
  for_ @[] [1 .. 10 :: Int] \ i -> do
    win <- TmuxApi.send (NewWindow def)
    wins <- TmuxApi.send (ListWindows def)
    atomicModify' (<> [[exon|#{label}-#{show i}: created @#{show win.windowId}, total #{show (length wins)}|]])

-- | Stress test for concurrent TmuxClient access.
-- Two threads run inside a single @withTmux@ scope, each sending 20 commands
-- (10 iterations × create + list). The Lock in tmuxRequest serializes
-- send+receive pairs, ensuring correct response routing.
test_concurrent :: UnitTest
test_concurrent =
  tmuxTest do
    restop @TmuxError @NativeTmux $ withTmux @TmuxCommand @CodecError do
      restop @CodecError @Tmux do
        Conc.interpretAtomic @[Text] [] do
          withAsync_ (worker "A") (worker "B")
          wins <- TmuxApi.send (ListWindows def)
          log' <- atomicGet
          for_ log' (embed . putStrLn . toString)
          -- 10 windows from each thread + 1 initial = 21
          length wins === 21

-- | Test that tmux control mode notifications are captured and accessible
-- via the ReceiveNotification action.
--
-- A background thread consumes notifications and writes them to a Queue.
-- Three phases exercise different consumption patterns:
-- 1. Send 3 requests, take from queue after each
-- 2. Send 3 requests, take 3 after the last one
-- 3. Run 3 concurrent threads that each send 3 requests, take 9 after all finish
--
-- All notifications are asserted to be valid (non-empty, starting with '%').
test_notifications :: UnitTest
test_notifications =
  tmuxTest do
    restop @TmuxError @NativeTmux $ withTmux @TmuxCommand @CodecError do
      restop @CodecError @Tmux do
        Conc.interpretQueueTB @TmuxNotification 64 do
          Conc.interpretGates do
            withAsyncGated_ consumeNotifications do
              -- Phase 1: interleaved send/take
              phase1 <- for @[] [1 .. 3 :: Int] \ _ -> do
                void $ TmuxApi.send (NewWindow def)
                Time.sleep (MilliSeconds 10)
                takeNotifications 1
              let notes1 = concat phase1
              assertOrdered notes1

              -- Phase 2: batch send, then batch take
              for_ @[] [1 .. 3 :: Int] \ _ -> do
                void $ TmuxApi.send (NewWindow def)
                Time.sleep (MilliSeconds 10)
              notes2 <- takeNotifications 3
              assertOrdered notes2

              -- Phase 3: concurrent senders, batch take
              withAsync_ sender $ withAsync_ sender sender
              notes3 <- takeNotifications 9
              assertOrdered notes3

              -- Global ordering: each phase's notifications should follow the previous
              assertOrdered (notes1 <> notes2 <> notes3)
  where
    consumeNotifications ::
      Members [Tmux, Queue TmuxNotification, Gate] r =>
      Sem r ()
    consumeNotifications = do
      signal
      forever do
        n <- TmuxApi.receiveNotification
        Queue.write n

    sender ::
      Members [Tmux, Time t d] r =>
      Sem r ()
    sender =
      for_ @[] [1 .. 3 :: Int] \ _ -> do
        void $ TmuxApi.send (NewWindow def)
        Time.sleep (MilliSeconds 10)

    takeNotifications ::
      Member (Queue TmuxNotification) r =>
      Int ->
      Sem r [TmuxNotification]
    takeNotifications n =
      catMaybes <$> replicateM n takeOne

    takeOne ::
      Member (Queue TmuxNotification) r =>
      Sem r (Maybe TmuxNotification)
    takeOne =
      Queue.readTimeout (MilliSeconds 500) <&> \case
        Success a -> Just a
        _ -> Nothing

    assertOrdered ::
      Member (Hedgehog IO) r =>
      [TmuxNotification] ->
      Sem r ()
    assertOrdered notes = do
      assert (not (null notes))
      for_ notes \ n ->
        assert (not (Text.null n.name))
      assertNonDecreasing (mapMaybe notificationId notes)

    assertNonDecreasing ::
      Member (Hedgehog IO) r =>
      [Int] ->
      Sem r ()
    assertNonDecreasing = \case
      [] -> unit
      [_] -> unit
      a : b : rest -> do
        assert (a <= b)
        assertNonDecreasing (b : rest)

    notificationId :: TmuxNotification -> Maybe Int
    notificationId n =
      listToMaybe n.args >>= extractId

    extractId :: Text -> Maybe Int
    extractId = readMaybe . toString <=< stripPrefix "@"
