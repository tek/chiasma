module Chiasma.Test.ConcurrentTest where

import Exon (exon)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (withAsync_)
import Polysemy.Test (UnitTest, (===))

import Chiasma.Codec.Data.Window (Window (..))
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxCommand (TmuxCommand (ListWindows, NewWindow))
import Chiasma.Data.TmuxError (TmuxError)
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
-- (10 iterations × create + list). The Lock in interpretTmuxProcessBuffered
-- serializes Send operations, ensuring correct response routing.
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
