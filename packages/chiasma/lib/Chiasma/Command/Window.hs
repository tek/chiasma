module Chiasma.Command.Window where

import Exon (exon)
import Path (Abs, Dir, Path)

import Chiasma.Codec.Data.Pane (Pane)
import Chiasma.Codec.Data.Window (Window (Window))
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.LayoutError (LayoutError (LayoutError))
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (ListWindows, NewWindow, SplitWindow))
import Chiasma.Data.TmuxId (SessionId, WindowId)
import Chiasma.Data.WindowParams (cwd, detach, name, target)
import qualified Chiasma.Data.WindowSelection as WindowSelection
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxApi (Tmux)

sameId :: WindowId -> Window -> Bool
sameId target (Window i _ _) = target == i

windows ::
  Member Tmux r =>
  Sem r [Window]
windows =
  Tmux.send (ListWindows WindowSelection.All)

window ::
  Member Tmux r =>
  WindowId ->
  Sem r (Maybe Window)
window windowId =
  find (sameId windowId) <$> windows

sessionWindows ::
  Member Tmux r =>
  SessionId ->
  Sem r [Window]
sessionWindows sid =
  Tmux.send (ListWindows (WindowSelection.InSession (Target.Session sid)))

-- TODO this could be ListWindow
newSessionWindow ::
  Members [Tmux, Stop LayoutError] r =>
  SessionId ->
  Sem r Window
newSessionWindow sid =
  sessionWindows sid >>= \case
    [w] -> pure w
    ws -> stop (LayoutError [exon|New session contains multiple windows: #{show ws}|])

doesWindowExist ::
  Member Tmux r =>
  WindowId ->
  Sem r Bool
doesWindowExist windowId =
  any (sameId windowId) <$> windows

newWindow ::
  Member Tmux r =>
  SessionId ->
  Ident ->
  Sem r Window
newWindow sid name =
  Tmux.send (NewWindow def { target = Target.Session sid, name = Just name })

splitWindowInDir ::
  Member Tmux r =>
  Path Abs Dir ->
  WindowId ->
  Sem r Pane
splitWindowInDir dir windowId =
  Tmux.send (SplitWindow def { target = Target.Window windowId, detach = True, cwd = Just dir } def)

splitWindow ::
  Member Tmux r =>
  WindowId ->
  Sem r Pane
splitWindow windowId =
  Tmux.send (SplitWindow def { target = Target.Window windowId, detach = True } def)
