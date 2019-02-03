module Chiasma.Test.Tmux(
  withTestTmux,
  tmuxSpec,
  tmuxGuiSpec,
  sleep,
  usleep,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.IO.Handle (Handle)
import GHC.Real (fromIntegral)
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
import UnliftIO.Exception (tryAny)
import UnliftIO.Temporary (withSystemTempDirectory)

import Chiasma.Monad.Stream (runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (write)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Test.File (fixture)
import Data.Functor (void)

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

windowWidth :: Int
windowWidth = 200

windowHeight :: Int
windowHeight = 51

testTmuxProcessConfig :: Bool -> FilePath -> FilePath -> Terminal -> IO (ProcessConfig () () ())
testTmuxProcessConfig gui socket confFile (Terminal handle pty) = do
  resizePty pty (windowWidth, windowHeight)
  let
    stream :: StreamSpec st ()
    stream = useHandleClose handle
    stdio = setStdin stream . setStdout stream . setStderr stream
    tmuxArgs = ["-S", socket, "-f", confFile]
    prc =
      if gui
      then proc "urxvt" (["-geometry", show windowWidth ++ "x" ++ show windowHeight, "-e", "tmux"] ++ tmuxArgs)
      else proc "tmux" tmuxArgs
  return $ stdio prc

killPid :: Integral a => a -> IO ()
killPid =
  void . tryAny . Signal.signalProcess Signal.killProcess . fromIntegral

killProcess :: TmuxNative -> Process () () () -> IO ()
killProcess api prc = do
  _ <- runTmux api $ Tmux.write "kill-server" []
  let handle = unsafeProcessHandle prc
  mayPid <- getPid handle
  maybe (return ()) killPid mayPid

-- FIXME find a way to wait for tmux deterministically instead of sleeping
-- if the first tmux control mode process from a TmuxProg runs before urxvt has started the server,
-- it will not use the test tmux.conf
-- maybe start tmux first, then urxvt?
runAndKillTmux :: (TmuxNative -> IO a) -> TmuxNative -> Process () () () -> IO a
runAndKillTmux thunk api prc = do
  sleep 0.2
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
