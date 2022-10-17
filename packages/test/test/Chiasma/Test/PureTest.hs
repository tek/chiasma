module Chiasma.Test.PureTest where

import Conc (interpretAtomic)
import Polysemy.Test (TestError (TestError), UnitTest, runTestAuto, (===))

import Chiasma.Codec.Data.Pane (Pane (Pane))
import Chiasma.Codec.Data.Window (Window (Window))
import qualified Chiasma.Data.PaneSelection as PaneSelection
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (ListPanes, ListWindows, NewWindow))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId (PaneId), WindowId (WindowId))
import Chiasma.Data.WindowParams (WindowParams (name))
import qualified Chiasma.Effect.TmuxApi as TmuxApi
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Interpreter.Pure (interpretTmuxPure)
import Chiasma.Tmux (withTmux_)

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

responses ::
  Member (AtomicState Int) r =>
  TmuxCommand a ->
  Sem r (Either Text a)
responses = \case
  ListPanes _ ->
    atomicState' \case
      0 -> (1, Right [Pane 0 240 60 0 0])
      i -> (i, Right [Pane 0 240 60 0 0, Pane 1 240 60 0 0, Pane 2 240 60 0 0])
  NewWindow _ -> pure (Right (Window 1 240 60))
  ListWindows _ -> pure (Right [Window 0 240 60, Window 1 240 60, Window 2 240 60])
  _ -> pure (Left "unexpected")

test_pureClient :: UnitTest
test_pureClient =
  runTestAuto $ interpretAtomic 0 $ interpretTmuxPure responses $ stopToErrorWith TestError $ mapStop @TmuxError show do
    result <- restop @_ @(Scoped _ _) (withTmux_ @TmuxCommand @Text prog)
    ([p0], [p0, p1, p2], [w0, w1, w2]) === result
  where
    p0 = p 0
    p1 = p 1
    p2 = p 2
    w0 = w 0
    w1 = w 1
    w2 = w 2
