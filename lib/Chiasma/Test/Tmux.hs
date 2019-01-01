module Chiasma.Test.Tmux(
  withTestTmux,
  tmuxSpec,
) where

import System.FilePath ((</>))
import System.Process.Typed (ProcessConfig, withProcess, proc, setStdin, setStdout, setStderr, useHandleClose)
import System.Posix.Pty (Pty, resizePty, createPty)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd)
import System.Posix.IO (fdToHandle)
import UnliftIO.Temporary (withSystemTempDirectory)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Test.File (fixture)

testTmuxProcessConfigAtPty :: FilePath -> FilePath -> Fd -> Pty -> IO (ProcessConfig () () ())
testTmuxProcessConfigAtPty socket confFile fd pty = do
  handle <- fdToHandle fd
  resizePty pty (1000, 1000)
  let stream = useHandleClose handle
  let stdio = setStdin stream . setStdout stream . setStderr stream
  return $ stdio $ proc "tmux" ["-S", socket, "-f", confFile]

testTmuxProcessConfig :: FilePath -> FilePath -> IO (ProcessConfig () () ())
testTmuxProcessConfig socket confFile = do
  (_, slave) <- openPseudoTerminal
  pty <- createPty slave
  maybe (error "couldn't spawn pty") (testTmuxProcessConfigAtPty socket confFile slave) pty

withTestTmux :: (TmuxNative -> IO a) -> FilePath -> IO a
withTestTmux thunk tempDir = do
  let socket = tempDir </> "tmux_socket"
  conf <- fixture "u" "tmux.conf"
  pc <- testTmuxProcessConfig socket conf
  withProcess pc (const $ thunk (TmuxNative socket))

tmuxSpec :: (TmuxNative -> IO a) -> IO a
tmuxSpec thunk =
  withSystemTempDirectory "chiasma-test" $ withTestTmux thunk
