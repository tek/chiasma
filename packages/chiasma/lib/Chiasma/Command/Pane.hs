{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module Chiasma.Command.Pane where

import Prelude hiding (output)

import qualified Chiasma.Codec.Data.Pane as Codec (Pane)
import qualified Chiasma.Codec.Data.PaneMode as Codec (PaneMode (PaneMode))
import Chiasma.Data.Axis (Axis)
import Chiasma.Data.CapturePaneParams (CaptureOutput (Stdout), escapeSequences, output, target)
import Chiasma.Data.CopyModeParams (target)
import Chiasma.Data.KillPaneParams (target)
import qualified Chiasma.Data.PaneSelection as PaneSelection
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (TmuxPanes)
import Chiasma.Data.PipePaneParams (command, target)
import Chiasma.Data.ResizePaneParams (ResizeAbsolute (ResizeAbsolute), absolute, target)
import Chiasma.Data.SelectParams (target)
import qualified Chiasma.Data.SendKeysParams as SendKeysParams
import Chiasma.Data.SendKeysParams (Key, keys, target)
import Chiasma.Data.SplitParams (JoinPaneParams (axis, detach, source, target))
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (
  TmuxCommand (CapturePane, CopyMode, KillPane, MovePane, PipePane, ResizePane, SelectPane, SendKeys),
  )
import Chiasma.Data.TmuxId (PaneId, WindowId)
import Chiasma.Data.View (View (View))
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxApi (Tmux)

panes ::
  Member (TmuxPanes a) r =>
  Sem r [a]
panes =
  Tmux.send (Panes.List PaneSelection.All)

pane ::
  Member (TmuxPanes a) r =>
  PaneId ->
  Sem r (Maybe a)
pane paneId =
  Tmux.send (Panes.Find paneId)

windowPanes ::
  Member (TmuxPanes a) r =>
  WindowId ->
  Sem r [a]
windowPanes windowId =
  Tmux.send (Panes.List (PaneSelection.InWindow (Target.Window windowId)))

firstWindowPane ::
  Member (TmuxPanes a) r =>
  WindowId ->
  Sem r a
firstWindowPane windowId =
  Tmux.send (Panes.First (PaneSelection.InWindow (Target.Window windowId)))

closePane ::
  Member Tmux r =>
  PaneId ->
  Sem r ()
closePane paneId =
  Tmux.send (KillPane def { target = Target.Pane paneId })

isPaneIdOpen ::
  Member (TmuxPanes Codec.Pane) r =>
  PaneId ->
  Sem r Bool
isPaneIdOpen paneId =
  isJust <$> pane paneId

isPaneOpen ::
  Member (TmuxPanes Codec.Pane) r =>
  View PaneId ->
  Sem r Bool
isPaneOpen (View _ (Just paneId)) =
  isPaneIdOpen paneId
isPaneOpen _ =
  pure False

movePane ::
  Member Tmux r =>
  PaneId ->
  PaneId ->
  Axis ->
  Sem r ()
movePane paneId refId axis =
  Tmux.send (MovePane def {
    detach = True,
    source = Just (Target.Pane paneId),
    target = Target.Pane refId,
    axis = Just axis
  })

resizePane ::
  Member Tmux r =>
  PaneId ->
  Axis ->
  Int ->
  Sem r ()
resizePane paneId axis size =
  Tmux.send (ResizePane def {
    target = Target.Pane paneId,
    absolute = Just (ResizeAbsolute axis size)
  })

sendKeys ::
  Member Tmux r =>
  PaneId ->
  [Key] ->
  Sem r ()
sendKeys paneId lines' =
  for_ lines' \ line ->
    Tmux.send (SendKeys def { target = Target.Pane paneId, keys = [line] })

pipePane ::
  Member Tmux r =>
  PaneId ->
  Text ->
  Sem r ()
pipePane paneId cmd =
  Tmux.schedule (PipePane def { target = Target.Pane paneId, command = Just cmd })

capturePane ::
  Member Tmux r =>
  PaneId ->
  Sem r [Text]
capturePane paneId =
  Tmux.send (CapturePane def { target = Target.Pane paneId, escapeSequences = True, output = Just Stdout})

selectPane ::
  Member Tmux r =>
  PaneId ->
  Sem r ()
selectPane paneId =
  Tmux.send (SelectPane def { target = Target.Pane paneId })

copyMode ::
  Member Tmux r =>
  PaneId ->
  Sem r ()
copyMode paneId =
  Tmux.send (CopyMode def { target = Target.Pane paneId })

paneMode ::
  Member (TmuxPanes Codec.PaneMode) r =>
  PaneId ->
  Sem r (Maybe Codec.PaneMode)
paneMode =
  pane

quitCopyMode ::
  Member Tmux r =>
  Member (TmuxPanes Codec.PaneMode) r =>
  PaneId ->
  Sem r ()
quitCopyMode paneId =
  traverse_ check =<< pane paneId
  where
    check (Codec.PaneMode _ mode) =
      when (mode == "copy-mode") do
        Tmux.send (SendKeys def { target = Target.Pane paneId, SendKeysParams.copyMode = True, keys = ["cancel"] })
