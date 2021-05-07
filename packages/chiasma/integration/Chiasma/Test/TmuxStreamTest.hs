module Chiasma.Test.TmuxStreamTest where

import Chiasma.Codec.Data (Pane(Pane), Window(Window))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(..), WindowId(..))
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Monad.Stream (runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (read, write)
import Chiasma.Native.Api (TmuxNative(..))
import Control.Monad.Free.Class (MonadFree)
import Hedgehog ((===))

import Chiasma.Test.Tmux (tmuxSpec)
import Chiasma.Test.Util (UnitTest)

prog :: MonadFree TmuxThunk m => m ([Pane], [Pane], [Window])
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
runProg api = runExceptT @TmuxError $ runTmux api prog

test_streamed :: UnitTest
test_streamed = do
  result <- liftIO (tmuxSpec runProg)
  (Right ([p0], [p0, p1, p2], [w0, w1, w2])) === result
  where
    p0 = p 0
    p1 = p 1
    p2 = p 2
    w0 = w 0
    w1 = w 1
    w2 = w 2
