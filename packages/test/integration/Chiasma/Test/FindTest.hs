module Chiasma.Test.FindTest where

import Polysemy.Test (UnitTest, assert, (===))

import Chiasma.Codec (TmuxCodec)
import qualified Chiasma.Codec.Data.Pane as Pane
import Chiasma.Codec.Data.Window (windowId)
import Chiasma.Command.Window (splitWindow)
import Chiasma.Data.CodecError (CodecError)
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand (NewWindow))
import qualified Chiasma.Effect.TmuxApi as TmuxApi
import Chiasma.Interpreter.Codec (interpretCodecPanes)
import Chiasma.Test.Tmux (tmuxTest)
import Chiasma.Tmux (withTmuxApis_)

data Pid =
  Pid { panePid :: Int }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)

test_find :: UnitTest
test_find = do
  tmuxTest $ interpretCodecPanes do
    (Pid pid, found) <- withTmuxApis_ @[TmuxCommand, Panes Pid] @CodecError do
      win <- TmuxApi.send (NewWindow def)
      pane <- splitWindow win.windowId
      p1 <- TmuxApi.send (Panes.Get (pane.paneId))
      p2 <- TmuxApi.send (Panes.Find 100)
      pure (p1, p2)
    assert (pid > 0)
    Nothing === found
