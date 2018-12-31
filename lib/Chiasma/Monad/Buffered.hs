module Chiasma.Monad.Buffered(
  runTmux,
) where

import GHC.IO.Exception (ExitCode(ExitSuccess))
import Control.Monad (when)
import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.Default.Class (Default(def))
import Data.List.Split (linesBy)
import System.Process.Typed (ProcessConfig, readProcessStdout, proc)
import UnliftIO (throwIO)
import Chiasma.Api (TmuxApi)
import Chiasma.Data.TmuxThunk (Cmd(..), Args(..), TmuxThunk(..), TmuxCommandFailed(..))
import Chiasma.Monad.Tmux (TmuxProg)

newtype TmuxState = TmuxState [[String]]

instance Default TmuxState where
  def = TmuxState def

tmuxProcessConfig :: [String] -> ProcessConfig () () ()
tmuxProcessConfig cmds =
  proc "tmux" ("-C" : cmds)

tmuxProcess :: MonadIO m => [[String]] -> m [String]
tmuxProcess args = do
  let cmds = fmap unwords args
  (code, out) <- readProcessStdout $ tmuxProcessConfig cmds
  let outLines = linesBy (=='\n') $ B.unpackChars out
  when (code /= ExitSuccess) $ throwIO (TmuxCommandFailed (Cmd "buffered") (Args cmds) outLines)
  return outLines

interpret :: MonadIO m => TmuxState -> TmuxProg a b -> m b
interpret (TmuxState cmds) (Pure a) = a <$ tmuxProcess cmds
interpret (TmuxState cmds) (Free (Read (Cmd cmd) (Args args) next)) = do
  a <- tmuxProcess $ (cmd : args) : cmds
  interpret def (next a)
interpret _ _ = undefined

runTmux :: (MonadIO m, TmuxApi c) => c -> TmuxProg a b -> m b
runTmux _ = interpret def
