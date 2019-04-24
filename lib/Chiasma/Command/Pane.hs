module Chiasma.Command.Pane where

import Data.Foldable (find, traverse_)
import Data.List (dropWhileEnd, intercalate)
import Data.List.Split (splitOn)
import Data.Text (Text)

import Chiasma.Codec (TmuxCodec)
import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Codec.Data.PaneCoords as Codec (PaneCoords)
import qualified Chiasma.Codec.Data.PanePid as Codec (PanePid)
import Chiasma.Data.TmuxId (HasPaneId, PaneId, WindowId, formatId)
import qualified Chiasma.Data.TmuxId as HasPaneId (paneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.View (View(View))
import qualified Chiasma.Monad.Tmux as Tmux (read, readRaw, unsafeReadFirst, write)
import Control.Monad.Free.Class (MonadFree)

paneTarget :: PaneId -> [String]
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

windowPanes :: MonadFree TmuxThunk m => WindowId -> m [Codec.Pane]
windowPanes windowId =
  Tmux.read "list-panes" ["-t", formatId windowId]

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
    send line = Tmux.write "send-keys" (paneTarget paneId ++ [line])

pipePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  String ->
  m ()
pipePane paneId cmd =
  Tmux.write "pipe-pane" (paneTarget paneId ++ [cmd])

capturePane ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m [Text]
capturePane paneId = do
  lines' <- Tmux.readRaw "capture-pane" (paneTarget paneId ++ ["-p", "-e"])
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
