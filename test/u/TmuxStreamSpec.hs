{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TmuxStreamSpec(
  htf_thisModulesTests
) where

import Control.Monad.Free.Class (MonadFree)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Test.Framework
import Chiasma.Codec.Data (Pane(Pane), Window(Window))
import Chiasma.Data.TmuxId (WindowId(..), PaneId(..))
import Chiasma.Data.TmuxThunk (TmuxThunk, TmuxError)
import Chiasma.Monad.Stream (runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (read, write)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Test.Tmux (tmuxSpec)

prog :: (MonadIO m, MonadFree TmuxThunk m) => m ([Pane], [Pane], [Window])
prog = do
  liftIO $ print "go"
  panes1 <- Tmux.read "list-panes" ["-t", "%0"]
  Tmux.write "new-window" []
  Tmux.write "new-window" []
  wins <- Tmux.read "list-windows" []
  panes <- Tmux.read "list-panes" ["-a"]
  return (panes1, panes, wins)

p :: Int -> Pane
p i = Pane (PaneId i) 1000 999

w :: Int -> Window
w i = Window (WindowId i) 1000 999

runProg :: TmuxNative -> IO (Either TmuxError ([Pane], [Pane], [Window]))
runProg api = runTmux api prog

test_streamed :: IO ()
test_streamed = do
  result <- tmuxSpec runProg
  assertEqual (Right ([p0], [p0, p1, p2], [w0, w1, w2])) result
  where
    p0 = p 0
    p1 = p 1
    p2 = p 2
    w0 = w 0
    w1 = w 1
    w2 = w 2
