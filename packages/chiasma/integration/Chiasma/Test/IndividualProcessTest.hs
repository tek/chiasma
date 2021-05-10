module Chiasma.Test.IndividualProcessTest where

import Chiasma.Codec.Data (Pane(Pane), Window(Window))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(..), WindowId(..))
import Chiasma.Monad.IndividualProcess (TmuxProg, runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (read, write)
import Chiasma.Native.Api (TmuxNative(..))
import Hedgehog ((===))

import Chiasma.Test.Tmux (tmuxTest)
import Chiasma.Test.Util (UnitTest)

prog :: TmuxProg ([Pane], [Pane], [Window])
prog = do
  panes1 <- Tmux.read "list-panes" ["-t", "%0"]
  Tmux.write "new-window" []
  Tmux.write "new-window" []
  wins <- Tmux.read "list-windows" []
  panes <- Tmux.read "list-panes" ["-a"]
  return (panes1, panes, wins)

p :: Int -> Pane
p i = Pane (PaneId i) 240 60

w :: Int -> Window
w i = Window (WindowId i) 240 60

runProg :: TmuxNative -> IO (Either TmuxError ([Pane], [Pane], [Window]))
runProg api = runExceptT $ runTmux api prog

test_individual :: UnitTest
test_individual = do
  result :: Either TmuxError ([Pane], [Pane], [Window]) <- liftIO (tmuxTest runProg)
  Right ([p0], [p0, p1, p2], [w0, w1, w2]) === result
  where
    p0 = p 0
    p1 = p 1
    p2 = p 2
    w0 = w 0
    w1 = w 1
    w2 = w 2
