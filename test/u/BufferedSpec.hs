{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BufferedSpec(
  htf_thisModulesTests
) where

import Data.Either (isLeft)
import Test.Framework
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Monad.Buffered (runTmux)
import Chiasma.Monad.Tmux (TmuxProg)
import qualified Chiasma.Monad.Tmux as Tmux (read, write)
import Chiasma.Test.Tmux (tmuxSpec)

prog :: TmuxProg a ([[String]], [[String]], [[String]])
prog = do
  panes1 <- Tmux.read "list-panes" ["-t", "%0"]
  Tmux.write "new-window" []
  Tmux.write "new-window" []
  wins <- Tmux.read "list-windows" []
  panes <- Tmux.read "list-panes" ["-a"]
  return (panes1, panes, wins)

runProg :: TmuxNative -> IO (Either TmuxError ([[String]], [[String]], [[String]]))
runProg api = runTmux api prog

test_buffered :: IO ()
test_buffered = do
  result <- tmuxSpec runProg
  print result
  assertEqual True (isLeft (result :: Either TmuxError ([[String]], [[String]], [[String]])))
