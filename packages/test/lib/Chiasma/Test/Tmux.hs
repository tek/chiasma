module Chiasma.Test.Tmux where

import qualified Chronos
import Chronos (datetimeToTime)
import Exon (exon)
import Hedgehog (TestT)
import Path (Abs, Dir, File, Path, relfile, (</>))
import Path.IO (createTempDir, doesFileExist, getTempDir, removeDirRecur)
import Polysemy.Chronos (ChronosTime, interpretTimeChronosConstant)
import qualified Polysemy.Conc as Race
import Polysemy.Conc (interpretRace)
import qualified Polysemy.Log as Log
import Polysemy.Log (interpretLogStdoutConc)
import Polysemy.Process (interpretSystemProcessNativeOpaqueSingle, resolveExecutable)
import qualified Polysemy.Process.Effect.Pty as Pty
import Polysemy.Process.Effect.Pty (Pty, withPty)
import qualified Polysemy.Process.Effect.SystemProcess as SystemProcess
import Polysemy.Process.Effect.SystemProcess (SystemProcess)
import Polysemy.Process.Interpreter.Pty (interpretPty)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, TestError (TestError), runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds), mkDatetime)
import System.Process.Typed (ProcessConfig, StreamSpec, proc, setStderr, setStdin, setStdout, useHandleClose)

import Chiasma.Codec.Data.Pane (Pane)
import Chiasma.Codec.Data.Session (Session (Session))
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxCommand (TmuxCommand (KillServer, ListSessions))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxNative (TmuxNative (..))
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Effect.Codec (Codec)
import qualified Chiasma.Effect.TmuxApi as TmuxApi
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Interpreter.Codec (interpretCodecPanesPane, interpretCodecTmuxCommand)
import Chiasma.Interpreter.TmuxClient (interpretTmuxNative)
import Chiasma.Path (pathText)
import Chiasma.Tmux (withTmux)

data TmuxTestConf =
  TmuxTestConf {
    ttcWidth :: Pty.Rows,
    ttcHeight :: Pty.Cols,
    ttcFontSize :: Int,
    ttcGui :: Bool,
    ttcConf :: [Text]
  }
  deriving stock (Eq, Show)

instance Default TmuxTestConf where
  def =
    TmuxTestConf 240 61 10 True mempty

urxvtArgs :: Int -> Int -> Int -> [Text]
urxvtArgs width height fontSize =
  ["-geometry", show width <> "x" <> show height, "-fn", "xft:monospace:size=" <> show fontSize, "-e", "tmux"]

createTmuxConf ::
  Member Test r =>
  Path Abs File ->
  [Text] ->
  Sem r (Path Abs File)
createTmuxConf wait content =
  Test.tempFile (defaultContent ++ content ++ initCommands) [relfile|tmux.conf|]
  where
    defaultContent =
      ["set -g default-command '/usr/bin/env bash --norc --noprofile'"]
    initCommands =
      [
        [exon|run-shell -b 'touch #{pathText wait}'|]
      ]

testTmuxProcessConfig ::
  Members [Pty, Test, Embed IO] r =>
  Path Abs File ->
  TmuxTestConf ->
  Path Abs File ->
  Sem r (ProcessConfig () () ())
testTmuxProcessConfig wait (TmuxTestConf width height fontSize gui conf) socket = do
  confFile <- createTmuxConf wait conf
  Pty.resize width height
  handle <- Pty.handle
  let
    tmuxArgs =
      ["-S", pathText socket, "-f", pathText confFile]
    prc =
      if gui
      then proc "urxvt" (toString <$> urxvtArgs (fromIntegral width) (fromIntegral height) fontSize ++ tmuxArgs)
      else proc "tmux" (toString <$> tmuxArgs)
  pure (stdio (useHandleClose handle) prc)
  where
    stdio (s :: ∀ st . StreamSpec st ()) =
      setStdin s . setStdout s . setStderr s

waitForServer ::
  ∀ resource enc dec t d r .
  Members [Scoped resource (TmuxClient enc dec) !! TmuxError, Codec TmuxCommand enc dec !! CodecError, Time t d] r =>
  Sem r ()
waitForServer =
  Time.while (MilliSeconds 10) do
    resumeAs @CodecError @(Codec _ _ _) True $ resumeAs @TmuxError @(Scoped _ _) True $ withTmux do
      s <- [] <! (TmuxApi.send ListSessions)
      pure (s /= [Session 0 "0"])

waitForFile ::
  Members [Time t d, Embed IO] r =>
  Path Abs File ->
  Sem r ()
waitForFile file =
  Time.while (MilliSeconds 10) do
    not <$> doesFileExist file

runAndKillTmux ::
  ∀ resource err enc dec t d r a .
  Members [Scoped resource (TmuxClient enc dec) !! TmuxError, Codec TmuxCommand enc dec !! CodecError] r =>
  Members [SystemProcess !! err, Time t d, Log, Resource, Error Text, Race, Embed IO] r =>
  Sem r a ->
  Sem r a
runAndKillTmux thunk = do
  Race.timeout (throw "tmux didn't create sessions") (Seconds 3) waitForServer
  result <- finally thunk do
    resumeWith @TmuxError (withTmux (resume_ (TmuxApi.send KillServer))) (Log.error "failed to kill server")
    resume_ SystemProcess.kill
  result <$ resume_ (void SystemProcess.wait)

type TestTmuxEffects =
  [
    Scoped () (TmuxClient (Const TmuxRequest) (Const [Text])) !! TmuxError,
    Codec TmuxCommand (Const TmuxRequest) (Const [Text]) !! CodecError,
    Codec (Panes Pane) (Const TmuxRequest) (Const [Text]) !! CodecError,
    Reader TmuxNative
  ]

withTestTmux ::
  Members [Test, Time t d, Log, Resource, Error Text, Race, Async, Embed IO] r =>
  TmuxTestConf ->
  Sem (TestTmuxEffects ++ r) a ->
  Path Abs Dir ->
  Sem r a
withTestTmux tConf thunk tempDir = do
  let socket = tempDir </> [relfile|tmux_socket|]
  let wait = tempDir </> [relfile|wait|]
  exe <- fromEither =<< resolveExecutable [relfile|tmux|] Nothing
  interpretPty $ resumeHoistError @_ @(Scoped _ _) show $ withPty do
    pc <- testTmuxProcessConfig wait tConf socket
    interpretSystemProcessNativeOpaqueSingle pc $ runReader (TmuxNative exe (Just socket)) do
      void $ Race.timeout (throw "tmux didn't start") (Seconds 3) (waitForFile wait)
      interpretCodecPanesPane $ interpretCodecTmuxCommand $ interpretTmuxNative do
        runAndKillTmux @() (insertAt @4 thunk)

withTempDir ::
  Members [Resource, Embed IO] r =>
  Path Abs Dir ->
  (Path Abs Dir -> Sem r a) ->
  Sem r a
withTempDir targetDir f =
  bracket
    (createTempDir targetDir "chiasma-test")
    (tryAny . removeDirRecur)
    f

withSystemTempDir ::
  Members [Resource, Embed IO] r =>
  (Path Abs Dir -> Sem r a) ->
  Sem r a
withSystemTempDir f = do
  targetDir <- getTempDir
  withTempDir targetDir f

type TestStack =
  TestTmuxEffects ++ [
    ChronosTime,
    Log,
    Stop CodecError,
    Error CodecError,
    Stop RenderError,
    Error RenderError,
    Stop TmuxError,
    Error TmuxError,
    Error Text,
    Race,
    Async,
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
    Embed IO,
    Resource,
    Final IO
  ]

testTime :: Chronos.Time
testTime =
  datetimeToTime (mkDatetime 2030 3 20 12 0 0)

runTmuxTest ::
  TmuxTestConf ->
  Sem TestStack a ->
  TestT IO a
runTmuxTest conf thunk =
  runTestAuto $
  asyncToIOFinal $
  interpretRace $
  mapError TestError $
  mapError show $
  stopToError $
  mapError @RenderError @Text show $
  stopToError $
  mapError @CodecError @Text show $
  stopToError $
  interpretLogStdoutConc $
  interpretTimeChronosConstant testTime do
    withSystemTempDir (withTestTmux conf thunk)

tmuxTest ::
  Sem TestStack a ->
  TestT IO a
tmuxTest =
  runTmuxTest def { ttcGui = False }

tmuxGuiTest ::
  Sem TestStack a ->
  TestT IO a
tmuxGuiTest =
  runTmuxTest def
