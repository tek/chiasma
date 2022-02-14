module Chiasma.Command.Pane where

import Control.Monad.Free.Class (MonadFree)
import Data.List (dropWhileEnd)
import qualified Data.Text as Text (intercalate, singleton, splitOn)

import Chiasma.Codec (TmuxCodec)
import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Codec.Data.PaneCoords as Codec (PaneCoords)
import qualified Chiasma.Codec.Data.PaneMode as Codec (PaneMode (PaneMode))
import qualified Chiasma.Codec.Data.PanePid as Codec (PanePid)
import Chiasma.Data.TmuxId (HasPaneId, PaneId, WindowId, formatId)
import qualified Chiasma.Data.TmuxId as HasPaneId (paneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.View (View (View))
import qualified Chiasma.Monad.Tmux as Tmux (read, readRaw, unsafeReadFirst, write)

paneTarget :: PaneId -> [Text]
paneTarget paneId =
  ["-t", formatId paneId]

sameId :: HasPaneId a => PaneId -> a -> Bool
sameId target candidate = target == HasPaneId.paneId candidate

panesAs :: (MonadFree TmuxThunk m, TmuxCodec a) => m [a]
panesAs =
  Tmux.read "list-panes" ["-a"]

panes :: MonadFree TmuxThunk m => m [Codec.Pane]
panes =
  panesAs

pane :: (MonadFree TmuxThunk m, TmuxCodec a, HasPaneId a) => PaneId -> m (Maybe a)
pane paneId =
  find (sameId paneId) <$> Tmux.read "list-panes" ["-t", formatId paneId]

windowPanesAs :: (MonadFree TmuxThunk m, TmuxCodec a) => WindowId -> m [a]
windowPanesAs windowId =
  Tmux.read "list-panes" ["-t", formatId windowId]

windowPanes :: MonadFree TmuxThunk m => WindowId -> m [Codec.Pane]
windowPanes =
  windowPanesAs

firstWindowPane :: MonadFree TmuxThunk m => WindowId -> m Codec.Pane
firstWindowPane windowId =
  Tmux.unsafeReadFirst "list-panes" ["-t", formatId windowId]

closePane :: MonadFree TmuxThunk m => PaneId -> m ()
closePane paneId =
  Tmux.write "kill-pane" (paneTarget paneId)

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

formatLine :: Text -> [Text]
formatLine line =
  ["\"" <> replace "\"" "\\\"" line <> Text.singleton '"', "enter"]
  where
    replace from to =
      Text.intercalate to . Text.splitOn from

sendKeys ::
  MonadFree TmuxThunk m =>
  PaneId ->
  [Text] ->
  [Text] ->
  m ()
sendKeys paneId args lines' =
  traverse_ send formatted
  where
    formatted = lines' >>= formatLine
    send line = Tmux.write "send-keys" (args <> paneTarget paneId <> [line])

pipePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  Text ->
  m ()
pipePane paneId cmd =
  Tmux.write "pipe-pane" (paneTarget paneId <> [cmd])

capturePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m [Text]
capturePane paneId = do
  lines' <- Tmux.readRaw "capture-pane" (paneTarget paneId <> ["-p", "-e", "-J"])
  return $ dropWhileEnd ("" ==) lines'

panePid ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m (Maybe Codec.PanePid)
panePid =
  pane

panePids ::
  MonadFree TmuxThunk m =>
  m [Codec.PanePid]
panePids =
  panesAs

paneCoords ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m (Maybe Codec.PaneCoords)
paneCoords =
  pane

selectPane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m ()
selectPane paneId =
  Tmux.write "select-pane" (paneTarget paneId)

copyMode ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m ()
copyMode =
  Tmux.write "copy-mode" . paneTarget

paneMode ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m (Maybe Codec.PaneMode)
paneMode =
  pane

quitCopyMode ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m ()
quitCopyMode paneId =
  traverse_ check =<< pane paneId
  where
    check (Codec.PaneMode _ mode) =
      when (mode == "copy-mode") (sendKeys paneId ["-X"] ["C-c"])
