module Chiasma.Command.Pane(
  windowPanes,
  closePane,
  firstWindowPane,
  panes,
  isPaneOpen,
  movePane,
  isPaneIdOpen,
  resizePane,
) where

import Control.Monad.Free.Class (MonadFree)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.TmuxId (WindowId, PaneId, formatId)
import Chiasma.Data.View (View(View))
import qualified Chiasma.Codec.Data as Codec (Pane(Pane))
import qualified Chiasma.Monad.Tmux as Tmux (read, unsafeReadFirst, write)

panes :: MonadFree TmuxThunk m => m [Codec.Pane]
panes =
  Tmux.read "list-panes" ["-a"]

windowPanes :: MonadFree TmuxThunk m => WindowId -> m [Codec.Pane]
windowPanes windowId =
  Tmux.read "list-panes" ["-t", formatId windowId]

firstWindowPane :: MonadFree TmuxThunk m => WindowId -> m Codec.Pane
firstWindowPane windowId =
  Tmux.unsafeReadFirst "list-panes" ["-t", formatId windowId]

closePane :: MonadFree TmuxThunk m => PaneId -> m ()
closePane paneId =
  Tmux.write "kill-pane" ["-t", formatId paneId]

isPaneIdOpen ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m Bool
isPaneIdOpen paneId =
  any (\(Codec.Pane i _ _) -> i == paneId) <$> panes

isPaneOpen ::
  MonadFree TmuxThunk m =>
  View PaneId ->
  m Bool
isPaneOpen (View _ (Just paneId)) =
  isPaneIdOpen paneId
isPaneOpen _ = return False

movePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  PaneId ->
  Bool ->
  m ()
movePane paneId refId vertical =
  Tmux.write "move-pane" ["-d", "-s", formatId paneId, "-t", formatId refId, direction]
  where
    direction = if vertical then "-v" else "-h"

resizePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  Bool ->
  Int ->
  m ()
resizePane paneId vertical size =
  Tmux.write "resize-pane" ["-t", formatId paneId, direction, show size]
  where
    direction = if vertical then "-y" else "-x"
