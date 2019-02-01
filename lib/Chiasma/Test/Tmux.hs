module Chiasma.Test.Tmux(
  withTestTmux,
  tmuxSpec,
  tmuxGuiSpec,
  sleep,
  usleep,
) where

import GHC.IO.Handle (Handle)
import GHC.Real (fromIntegral)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))
import System.Posix.IO (fdToHandle)
import System.Posix.Pty (Pty, resizePty, createPty)
import qualified System.Posix.Signals as Signal (signalProcess, killProcess)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process (getPid)
import System.Process.Typed (
  ProcessConfig,
  Process,
  StreamSpec,
  withProcess,
  proc,
  setStdin,
  setStdout,
  setStderr,
  useHandleClose,
  unsafeProcessHandle,
  )
import UnliftIO (finally, throwString)
import UnliftIO.Temporary (withSystemTempDirectory)

import Chiasma.Monad.Stream (runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (write)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Test.File (fixture)

data Terminal = Terminal Handle Pty

usleep :: MonadIO f => Double -> f ()
usleep useconds =
  liftIO $ threadDelay $ round $ useconds

sleep :: MonadIO f => Double -> f ()
sleep seconds =
  usleep $ seconds * 1e6

unsafeTerminal :: IO Terminal
unsafeTerminal = do
  (_, slave) <- openPseudoTerminal
  mayPty <- createPty slave
  handle <- fdToHandle slave
  pty <- maybe (throwString "couldn't spawn pty") return mayPty
  return $ Terminal handle pty

testTmuxProcessConfig :: Bool -> FilePath -> FilePath -> Terminal -> IO (ProcessConfig () () ())
testTmuxProcessConfig gui socket confFile (Terminal handle pty) = do
  resizePty pty (1000, 1000)
  let
    stream :: StreamSpec st ()
    stream = useHandleClose handle
    stdio = setStdin stream . setStdout stream . setStderr stream
    prc =
      if gui
      then proc "urxvt" ["-geometry", "1000x1000", "-e", "tmux", "-f", confFile, "-S", socket]
      else proc "tmux" ["-S", socket, "-f", confFile]
  return $ stdio prc

killPid :: Integral a => a -> IO ()
killPid =
  Signal.signalProcess Signal.killProcess . fromIntegral

killProcess :: TmuxNative -> Process () () () -> IO ()
killProcess api prc = do
  _ <- runTmux api $ Tmux.write "kill-server" []
  let handle = unsafeProcessHandle prc
  mayPid <- getPid handle
  maybe (return ()) killPid mayPid

runAndKillTmux :: (TmuxNative -> IO a) -> TmuxNative -> Process () () () -> IO a
runAndKillTmux thunk api prc = do
  sleep 0.1
  finally (thunk api) (killProcess api prc)

withTestTmux :: Bool -> (TmuxNative -> IO a) -> FilePath -> IO a
withTestTmux gui thunk tempDir = do
  let socket = tempDir </> "tmux_socket"
  conf <- fixture "u" "tmux.conf"
  terminal <- unsafeTerminal
  pc <- testTmuxProcessConfig gui socket conf terminal
  withProcess pc $ runAndKillTmux thunk (TmuxNative $ Just socket)

tmuxSpec' :: Bool -> (TmuxNative -> IO a) -> IO a
tmuxSpec' gui thunk =
  withSystemTempDirectory "chiasma-test" $ withTestTmux gui thunk

tmuxSpec :: (TmuxNative -> IO a) -> IO a
tmuxSpec =
  tmuxSpec' False

tmuxGuiSpec :: (TmuxNative -> IO a) -> IO a
tmuxGuiSpec =
  tmuxSpec' True
