module Chiasma.Api(
  TmuxApi(..),
  TmuxNative(..),
) where

import GHC.IO.Exception (ExitCode(ExitSuccess))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
import Data.List.Split (linesBy)
import System.Process.Typed (ProcessConfig, readProcessStdout, proc)
import UnliftIO (throwIO)
import Chiasma.Data.TmuxThunk (Cmd(..), Args(..), TmuxCommandFailed(..))

class TmuxApi a where
  runCommands :: (MonadIO m) => a -> [(Cmd, Args)] -> m [String]

newtype TmuxNative = TmuxNative FilePath

tmuxProcessConfig :: FilePath -> [String] -> ProcessConfig () () ()
tmuxProcessConfig socket cmds =
  proc "tmux" ("-S" : socket : "-C" : cmds)

nativeTmuxProcess :: MonadIO m => FilePath -> [[String]] -> m [String]
nativeTmuxProcess socket args = do
  let cmds = fmap unwords args
  (code, out) <- readProcessStdout $ tmuxProcessConfig socket cmds
  let outLines = linesBy (=='\n') $ B.unpackChars out
  when (code /= ExitSuccess) $ throwIO (TmuxCommandFailed (Cmd "buffered") (Args cmds) outLines)
  return outLines

formatCmds :: [(Cmd, Args)] -> [[String]]
formatCmds = (build <$>)
  where
    build (Cmd cmd, Args args) = cmd : args

instance TmuxApi TmuxNative where
  runCommands (TmuxNative socket) cmds = nativeTmuxProcess socket $ formatCmds cmds
