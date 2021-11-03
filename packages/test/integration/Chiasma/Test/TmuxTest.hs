module Chiasma.Test.TmuxTest where

import Polysemy.Test (UnitTest, (===))

import Chiasma.Codec.Data.Pane (Pane (Pane))
import Chiasma.Codec.Data.Window (Window (Window))
import Chiasma.Data.CodecError (CodecError)
import qualified Chiasma.Data.PaneSelection as PaneSelection
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (ListPanes, ListWindows, NewWindow))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId (PaneId), WindowId (WindowId))
import Chiasma.Data.WindowParams (WindowParams (name))
import qualified Chiasma.Effect.TmuxApi as TmuxApi
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Test.Tmux (tmuxTest)
import Chiasma.Tmux (withTmux)

prog ::
  Sem (Tmux : r) ([Pane], [Pane], [Window])
prog = do
  panes1 <- TmuxApi.send (ListPanes (PaneSelection.InWindow (Target.Pane 0)))
  TmuxApi.send (NewWindow def)
  TmuxApi.send (NewWindow def { name = Just "second" })
  wins <- TmuxApi.send (ListWindows def)
  panes <- TmuxApi.send (ListPanes PaneSelection.All)
  pure (panes1, panes, wins)

p :: Int -> Pane
p i =
  Pane (PaneId i) 240 60 0 0

w :: Int -> Window
w i =
  Window (WindowId i) 240 60

test_tmux :: UnitTest
test_tmux = do
  tmuxTest do
    result <- restop @TmuxError @NativeTmux (withTmux (restop @CodecError @Tmux prog))
    ([p0], [p0, p1, p2], [w0, w1, w2]) === result
  where
    p0 = p 0
    p1 = p 1
    p2 = p 2
    w0 = w 0
    w1 = w 1
    w2 = w 2
