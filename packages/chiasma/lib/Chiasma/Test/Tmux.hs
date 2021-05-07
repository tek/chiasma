module Chiasma.Test.Tmux where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Monad.Stream (runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (write)
import Chiasma.Native.Api (TmuxNative(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (bracket, finally, ioError)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (userError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.IO (fdToHandle)
import System.Posix.Pty (Pty, createPty, resizePty)
import qualified System.Posix.Signals as Signal (killProcess, signalProcess)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process (getPid)
import System.Process.Typed (
  Process,
  ProcessConfig,
  StreamSpec,
  proc,
  setStderr,
  setStdin,
  setStdout,
  startProcess,
  stopProcess,
  unsafeProcessHandle,
  useHandleClose,
  waitExitCode,
  )
import UnliftIO.Exception (tryAny)

import Chiasma.Test.File (fixture)

data Terminal = Terminal Handle Pty

data TmuxTestConf =
  TmuxTestConf {
    ttcWidth :: Int,
    ttcHeight :: Int,
    ttcFontSize :: Int,
    ttcGui :: Bool
  }
  deriving (Eq, Show)

instance Default TmuxTestConf where
  def = TmuxTestConf 240 61 18 True

usleep :: MonadIO f => Double -> f ()
usleep =
  liftIO . threadDelay . round

sleep :: MonadIO f => Double -> f ()
sleep seconds =
  usleep $ seconds * 1e6

unsafeTerminal :: IO Terminal
unsafeTerminal = do
  (_, slave) <- openPseudoTerminal
  mayPty <- createPty slave
  handle <- fdToHandle slave
  pty <- maybe (ioError (userError "couldn't spawn pty")) return mayPty
  return $ Terminal handle pty

urxvtArgs :: Int -> Int -> Int -> [Text]
urxvtArgs width height fontSize =
  ["-geometry", show width <> "x" <> show height, "-fn", "xft:monospace:size=" <> show fontSize, "-e", "tmux"]

testTmuxProcessConfig :: TmuxTestConf -> FilePath -> FilePath -> Terminal -> IO (ProcessConfig () () ())
testTmuxProcessConfig (TmuxTestConf width height fontSize gui) socket confFile (Terminal handle pty) = do
  confFileExists <- doesFileExist confFile
  resizePty pty (width, height)
  let
    stream :: StreamSpec st ()
    stream = useHandleClose handle
    stdio = setStdin stream . setStdout stream . setStderr stream
    tmuxArgs = ["-S", toText socket, "-f", toText confFileArg]
    confFileArg = if confFileExists then confFile else "/dev/null"
    prc =
      if gui
      then proc "urxvt" (toString <$> urxvtArgs width height fontSize ++ tmuxArgs)
      else proc "tmux" (toString <$> tmuxArgs)
  return $ stdio prc

killPid :: Integral a => a -> IO ()
killPid =
  void . tryAny . Signal.signalProcess Signal.killProcess . fromIntegral

killProcess :: TmuxNative -> Process () () () -> IO ()
killProcess api prc = do
  _ <- runExceptT @TmuxError $ runTmux api $ Tmux.write "kill-server" []
  let handle = unsafeProcessHandle prc
  mayPid <- getPid handle
  maybe (return ()) killPid mayPid

-- FIXME find a way to wait for tmux deterministically instead of sleeping
-- if the first tmux control mode process from a TmuxProg runs before urxvt has started the server,
-- it will not use the test tmux.conf
-- maybe start tmux first, then urxvt?
runAndKillTmux ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (TmuxNative -> m a) ->
  TmuxNative ->
  Process () () () ->
  m a
runAndKillTmux thunk api prc = do
  liftIO (sleep 0.2)
  finally (thunk api) (liftIO (killProcess api prc))

withProcessWait ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessWait config f =
  bracket (startProcess config) stopProcess \ p -> f p <* waitExitCode p

withTestTmux ::
  MonadIO m =>
  MonadBaseControl IO m =>
  TmuxTestConf ->
  (TmuxNative -> m a) ->
  FilePath ->
  m a
withTestTmux tConf thunk tempDir = do
  let socket = tempDir </> "tmux_socket"
  conf <- fixture "u" "tmux.conf"
  terminal <- liftIO unsafeTerminal
  pc <- liftIO $ testTmuxProcessConfig tConf socket conf terminal
  withProcessWait pc $ runAndKillTmux thunk (TmuxNative $ Just socket)

withTempDir ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (FilePath -> m a) ->
  m a
withTempDir f = do
  targetDir <- liftIO getCanonicalTemporaryDirectory
  bracket
    (liftIO (createTempDirectory targetDir "chiasma-test"))
    (liftIO . removeDirectoryRecursive)
    f

tmuxSpec' ::
  MonadIO m =>
  MonadBaseControl IO m =>
  TmuxTestConf ->
  (TmuxNative -> m a) ->
  m a
tmuxSpec' conf thunk = do
  withTempDir (withTestTmux conf thunk)

tmuxSpec ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (TmuxNative -> m a) ->
  m a
tmuxSpec =
  tmuxSpec' def { ttcGui = False }

tmuxGuiSpec ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (TmuxNative -> m a) ->
  m a
tmuxGuiSpec =
  tmuxSpec' def
