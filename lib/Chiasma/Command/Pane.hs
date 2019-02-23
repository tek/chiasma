module Chiasma.Command.Pane(
  windowPanes,
  closePane,
  firstWindowPane,
  panes,
  isPaneOpen,
  movePane,
  isPaneIdOpen,
  resizePane,
  sendKeys,
  pipePane,
  sameId,
) where

import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import qualified Chiasma.Codec.Data as Codec (Pane(Pane))
import Chiasma.Data.TmuxId (WindowId, PaneId, formatId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.View (View(View))
import qualified Chiasma.Monad.Tmux as Tmux (read, unsafeReadFirst, write)
import Control.Monad.Free.Class (MonadFree)

sameId :: PaneId -> Codec.Pane -> Bool
sameId target (Codec.Pane i _ _) = target == i

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
  any (sameId paneId) <$> panes

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

formatLine :: String -> [String]
formatLine line =
  [['"'] ++ replace ['"'] ['\\', '"'] line ++ ['"'], "enter"]
  where
    replace from to = intercalate to . splitOn from

sendKeys ::
  MonadFree TmuxThunk m =>
  PaneId ->
  [String] ->
  m ()
sendKeys paneId lines' =
  traverse_ send formatted
  where
    formatted = lines' >>= formatLine
    send line = Tmux.write "send-keys" ["-t", formatId paneId, line]

pipePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  String ->
  m ()
pipePane paneId cmd =
  Tmux.write "pipe-pane" ["-t", formatId paneId, cmd]
