module Chiasma.Test.Tmux where

import Control.Concurrent (threadDelay)
import GHC.IO.Handle (Handle)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
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
  unsafeProcessHandle,
  useHandleClose,
  withProcess,
  )
import UnliftIO (finally, throwString)
import UnliftIO.Exception (tryAny)
import UnliftIO.Temporary (withSystemTempDirectory)

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Monad.Stream (runTmux)
import qualified Chiasma.Monad.Tmux as Tmux (write)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Test.File (fixture)
import Data.Functor (void)

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
  pty <- maybe (throwString "couldn't spawn pty") return mayPty
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
runAndKillTmux :: (TmuxNative -> IO a) -> TmuxNative -> Process () () () -> IO a
runAndKillTmux thunk api prc = do
  sleep 0.2
  finally (thunk api) (killProcess api prc)

withTestTmux :: TmuxTestConf -> (TmuxNative -> IO a) -> FilePath -> IO a
withTestTmux tConf thunk tempDir = do
  let socket = tempDir </> "tmux_socket"
  conf <- fixture "u" "tmux.conf"
  terminal <- unsafeTerminal
  pc <- testTmuxProcessConfig tConf socket conf terminal
  withProcess pc $ runAndKillTmux thunk (TmuxNative $ Just socket)

tmuxSpec' :: TmuxTestConf -> (TmuxNative -> IO a) -> IO a
tmuxSpec' conf thunk =
  withSystemTempDirectory "chiasma-test" $ withTestTmux conf thunk

tmuxSpec :: (TmuxNative -> IO a) -> IO a
tmuxSpec =
  tmuxSpec' def { ttcGui = False }

tmuxGuiSpec :: (TmuxNative -> IO a) -> IO a
tmuxGuiSpec =
  tmuxSpec' def
