{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BufferedSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Data.Pane (Pane(Pane), PaneId(..))
import Chiasma.Data.Window (Window(Window), WindowId(..))
import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Monad.Buffered (runTmux)
import Chiasma.Monad.Tmux (TmuxProg)
import qualified Chiasma.Monad.Tmux as Tmux (read, write)
import Chiasma.Test.Tmux (tmuxSpec)

prog :: TmuxProg ([Pane], [Pane], [Window])
prog = do
  panes1 <- Tmux.read "list-panes" ["-t", "%0"]
  Tmux.write "new-window" []
  Tmux.write "new-window" []
  wins <- Tmux.read @Window "list-windows" []
  panes <- Tmux.read "list-panes" ["-a"]
  return (panes1, panes, wins)

p :: Int -> Pane
p i = Pane (PaneId i) 1000 999

w :: Int -> Window
w i = Window (WindowId i) 1000 999

runProg :: TmuxNative -> IO (Either TmuxError ([Pane], [Pane], [Window]))
runProg api = runTmux api prog

test_buffered :: IO ()
test_buffered = do
  result <- tmuxSpec runProg
  assertEqual (Right ([p0], [p0, p1, p2], [w0, w1, w2])) result
  where
    p0 = p 0
    p1 = p 1
    p2 = p 2
    w0 = w 0
    w1 = w 1
    w2 = w 2
