module Chiasma.Monad.Simple(
  -- tmuxProcessConfig,
  -- tmuxProcess,
  -- interpret,
  -- runTmux,
) where

-- import GHC.IO.Exception (ExitCode(ExitSuccess))
-- import Control.Monad (when)
-- import Control.Monad.Free (foldFree)
-- import Control.Monad.IO.Class (MonadIO)
-- import Control.Monad.Trans.Except (ExceptT(ExceptT), catchE, runExceptT)
-- import qualified Data.ByteString.Lazy.Internal as B (unpackChars)
-- import Data.Functor (void)
-- import Data.List.Split (linesBy)
-- import System.Process.Typed (ProcessConfig, readProcessStdout, proc)
-- import UnliftIO (throwIO)
-- import Chiasma.Data.TmuxThunk (Cmd(..), CmdName(..), CmdArgs(..), Cmds(..), TmuxThunk(..), TmuxError(..))
-- import Chiasma.Monad.Tmux (TmuxProg)

-- tmuxProcessConfig :: CmdName -> CmdArgs -> ProcessConfig () () ()
-- tmuxProcessConfig (CmdName cmd) (CmdArgs args) =
--   proc "tmux" (["-C", cmd] ++ args)

-- tmuxProcess :: MonadIO m => Cmd -> m (Either TmuxError [String])
-- tmuxProcess cmd@(Cmd name args) = do
--   (code, out) <- readProcessStdout $ tmuxProcessConfig name args
--   let outLines = linesBy (=='\n') $ B.unpackChars out
--   return $ case code of
--     ExitSuccess -> Right outLines
--     _ -> Left $ TmuxProcessFailed (Cmds [cmd]) outLines

-- interpret :: MonadIO m => TmuxThunk a next -> ExceptT TmuxError m next
-- interpret (Read cmd next) = next <$> tmuxProcess cmd
-- interpret (Write cmd next) = next <$> void (tmuxProcess cmd)

-- runTmux :: MonadIO m => TmuxProg a next -> m (Either TmuxError next)
-- runTmux = runExceptT . foldFree interpret
