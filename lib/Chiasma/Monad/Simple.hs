module Chiasma.Monad.Simple(
  tmuxProcessConfig,
  tmuxProcess,
  interpret,
  runTmux,
) where

import GHC.IO.Exception (ExitCode(ExitSuccess))
import Control.Monad (when)
import Control.Monad.Free (foldFree)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.Functor (void)
import Data.List.Split (linesBy)
import System.Process.Typed (ProcessConfig, readProcessStdout, proc)
import UnliftIO (throwIO)
import Chiasma.Data.TmuxThunk (Cmd(..), Args(..), TmuxThunk(..), TmuxCommandFailed(..))
import Chiasma.Monad.Tmux (TmuxProg)

tmuxProcessConfig :: Cmd -> Args -> ProcessConfig () () ()
tmuxProcessConfig (Cmd cmd) (Args args) =
  proc "tmux" (["-C", cmd] ++ args)

tmuxProcess :: MonadIO m => Cmd -> Args -> m [String]
tmuxProcess cmd args = do
  (code, out) <- readProcessStdout $ tmuxProcessConfig cmd args
  let outLines = linesBy (=='\n') $ B.unpackChars out
  when (code /= ExitSuccess) $ throwIO (TmuxCommandFailed cmd args outLines)
  return outLines

interpret :: MonadIO m => TmuxThunk a next -> m next
interpret (Read cmd args next) = next <$> tmuxProcess cmd args
interpret (Write cmd args next) = next <$> void (tmuxProcess cmd args)

runTmux :: MonadIO m => TmuxProg a next -> m next
runTmux = foldFree interpret
