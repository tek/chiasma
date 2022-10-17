module Chiasma.Window where

import qualified Data.List.NonEmpty as NonEmpty (head, nonEmpty)
import Path (Abs, Dir, Path, parseAbsDir)
import Prettyprinter (line, pretty, vsep, (<+>))

import qualified Chiasma.Codec.Data.Pane as Pane
import qualified Chiasma.Codec.Data.Pane as Codec (Pane (Pane, paneId))
import Chiasma.Codec.Data.Pane (Pane (Pane))
import qualified Chiasma.Codec.Data.Window as Codec (Window (Window, windowId))
import qualified Chiasma.Command.Pane as Cmd (closePane, firstWindowPane, windowPanes)
import qualified Chiasma.Command.Window as Cmd (newWindow, splitWindowInDir, window)
import Chiasma.Data.Axis (Axis)
import Chiasma.Data.Ident (Ident, identText, identify)
import Chiasma.Data.Panes (TmuxPanes)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError (NoPrincipal))
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import qualified Chiasma.Data.View as Tmux (View (View))
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState (..))
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Pane (addPane)
import Chiasma.Ui.Data.RenderableTree (
  RLayout (..),
  RPane (..),
  Renderable (..),
  RenderableNode,
  RenderableTree,
  )
import qualified Chiasma.Ui.Data.Tree as Tree (Node (Leaf, Sub), Tree (Tree))
import Chiasma.Ui.Data.View (Tree (..), TreeSub (..), ViewTree, ViewTreeSub)
import qualified Chiasma.Ui.Data.View as Ui (Layout (..), Pane (Pane), PaneView, View (View))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (ViewGeometry, position))
import Chiasma.Ui.Data.ViewState (ViewState)
import Chiasma.View (findOrCreateView, viewsLog, viewsLogS)
import qualified Chiasma.View as Views (insertPane, insertWindow, pane, paneById, updatePane, updateWindow, window)

findOrCreateWindow ::
  Member (AtomicState Views) r =>
  Ident ->
  Sem r (Tmux.View WindowId)
findOrCreateWindow =
  findOrCreateView Views.window Views.insertWindow

registerWindowId ::
  Member (AtomicState Views) r =>
  Ident ->
  WindowId ->
  Sem r ()
registerWindowId ident windowId =
  atomicModify' (Views.updateWindow (Tmux.View ident (Just windowId)))

spawnWindow ::
  Members [AtomicState Views, Tmux] r =>
  SessionId ->
  Ident ->
  Sem r Codec.Window
spawnWindow sid ident = do
  win@(Codec.Window windowId _ _) <- Cmd.newWindow sid ident
  registerWindowId ident windowId
  viewsLogS $ "spawned window in session " <> show sid <> " with id " <> show windowId
  pure win

findPrincipalSub :: ViewTreeSub -> Maybe Ui.PaneView
findPrincipalSub (TreeNode t) = findPrincipal t
findPrincipalSub (TreeLeaf p@(Ui.View _ _ _ (Ui.Pane True _ _))) = Just p
findPrincipalSub _ = Nothing

findPrincipal :: ViewTree -> Maybe Ui.PaneView
findPrincipal (Tree _ sub) =
  firstJust findPrincipalSub sub

principalPane ::
  Members [AtomicState Views, Tmux, Stop RenderError] r =>
  ViewTree ->
  Sem r (Ui.PaneView, Tmux.View PaneId)
principalPane tree = do
  uiPane@(Ui.View uiPaneIdent _ _ _) <- stopNote (RenderError.NoPrincipal (identify tree)) $ (findPrincipal tree)
  existingTmuxPane <- atomicGets (Views.pane uiPaneIdent)
  tmuxPane <- either (const $ addPane uiPaneIdent) pure existingTmuxPane
  pure (uiPane, tmuxPane)

syncPrincipal ::
  Members [TmuxPanes Pane, AtomicState Views, Tmux, Stop RenderError] r =>
  WindowId ->
  ViewTree ->
  Sem r ()
syncPrincipal windowId tree@(Tree (Ui.View layoutIdent _ _ _) _) = do
  (Codec.Pane paneId _ _ _ _) <- Cmd.firstWindowPane windowId
  existing <- atomicGets (Views.paneById paneId)
  case existing of
    Nothing -> do
      (_, Tmux.View paneIdent _) <- principalPane tree
      viewsLog $ "setting principal of layout" <+> pretty layoutIdent <+> " to pane " <+> pretty paneIdent <+> "/" <+>
        pretty paneId
      atomicModify' $ Views.updatePane (Tmux.View paneIdent (Just paneId))
    _ -> pure ()

ensureWindow ::
  Members [TmuxPanes Pane, AtomicState Views, Tmux, Stop RenderError] r =>
  SessionId ->
  Tmux.View WindowId ->
  Maybe WindowId ->
  ViewTree ->
  Sem r Codec.Window
ensureWindow sid (Tmux.View ident mayWid) newSessionWid tree = do
  preexisting <- join <$> traverse Cmd.window (newSessionWid <|> mayWid)
  window <- maybe (spawnWindow sid ident) pure preexisting
  syncPrincipal (Codec.windowId window) tree
  pure window

findOrCreatePane ::
  Member (AtomicState Views) r =>
  Ident ->
  Sem r (Tmux.View PaneId)
findOrCreatePane =
  findOrCreateView Views.pane Views.insertPane

nativePane ::
  Member (TmuxPanes Pane) r =>
  WindowId ->
  Tmux.View PaneId ->
  Sem r (Maybe Pane)
nativePane windowId (Tmux.View _ (Just paneId)) =
  find sameId <$> Cmd.windowPanes windowId
  where
    sameId (Pane i _ _ _ _) = i == paneId
nativePane _ _ = pure Nothing

openPane ::
  Members [AtomicState Views, Tmux] r =>
  Path Abs Dir ->
  WindowId ->
  Sem r Pane
openPane dir windowId = do
  detail <- Cmd.splitWindowInDir dir windowId
  viewsLogS $ "opened pane " <> show (Pane.paneId detail) <> " in window " <> show windowId
  pure detail

ensurePaneOpen ::
  Members [AtomicState Views, Tmux] r =>
  Path Abs Dir ->
  Maybe Pane ->
  WindowId ->
  Sem r Pane
ensurePaneOpen _ (Just detail) _ =
  pure detail
ensurePaneOpen dir Nothing windowId =
  openPane dir windowId

ensurePaneClosed ::
  Members [AtomicState Views, Tmux] r =>
  Maybe Pane ->
  Sem r ()
ensurePaneClosed (Just (Pane i _ _ _ _)) = do
  viewsLogS $ "closing pane " <> show i
  Cmd.closePane i
ensurePaneClosed _ = pure ()

ensurePane ::
  Members [TmuxPanes Pane, AtomicState Views, Tmux] r =>
  Path Abs Dir ->
  WindowId ->
  Ui.PaneView ->
  Sem r (Maybe RenderableNode)
ensurePane cwd windowId (Ui.View paneIdent vState geometry (Ui.Pane open _ customDir)) = do
  tmuxPane <- findOrCreatePane paneIdent
  existingPane <- nativePane windowId tmuxPane
  updatedPane <-
    if open then Just <$> ensurePaneOpen dir existingPane windowId
    else Nothing <$ ensurePaneClosed existingPane
  atomicModify' $ Views.updatePane (Tmux.View paneIdent (Pane.paneId <$> updatedPane))
  pure $ cons <$> updatedPane
  where
    dir = fromMaybe cwd (parseAbsDir . toString =<< customDir)
    cons (Pane i _ _ top left) =
      Tree.Leaf . Renderable vState geometry $ RPane i top left

refPane :: RenderableNode -> RPane
refPane (Tree.Sub (Tree.Tree (Renderable _ _ (RLayout ref _)) _)) = ref
refPane (Tree.Leaf (Renderable _ _ pane)) = pane

renderableTree ::
  ViewState ->
  ViewGeometry ->
  Axis ->
  [RenderableNode] ->
  Maybe RenderableTree
renderableTree vState geometry axis sub = do
  sub' <- NonEmpty.nonEmpty sub
  pure $ Tree.Tree (Renderable vState geometry (RLayout (refPane $ NonEmpty.head sub') axis)) sub'

viewPosition :: ViewTreeSub -> Float
viewPosition (TreeNode (Tree (Ui.View _ _ ViewGeometry { position = pos } _) _)) =
  fromMaybe 0.5 pos
viewPosition (TreeLeaf (Ui.View _ _ ViewGeometry { position = pos } _)) =
  fromMaybe 0.5 pos

ensureView ::
  Members [TmuxPanes Pane, AtomicState Views, Tmux] r =>
  Path Abs Dir ->
  WindowId ->
  ViewTree ->
  Sem r (Maybe RenderableTree)
ensureView cwd windowId =
  ensureTree
  where
    ensureTree (Tree (Ui.View layoutIdent vState geometry (Ui.Layout axis)) sub) = do
      ensuredSub <- traverse ensureNode sortedSub
      viewsLog $ pretty ("new sub for layout `" <> identText layoutIdent <> "`:") <> line <>
        vsep (pretty <$> ensuredSub)
      pure $ renderableTree vState geometry axis $ catMaybes ensuredSub
      where
        sortedSub = sortOn viewPosition sub
    ensureNode (TreeNode t) = do
      newTree <- ensureTree t
      pure $ Tree.Sub <$> newTree
    ensureNode (TreeLeaf v) =
      ensurePane cwd windowId v

windowState ::
  Member (TmuxPanes Pane) r =>
  Ident ->
  Codec.Window ->
  RenderableTree ->
  Sem r WindowState
windowState windowIdent window tree = do
  nativeRef <- Cmd.firstWindowPane (Codec.windowId window)
  pure $ WindowState window nativeRef windowIdent tree (Codec.paneId nativeRef)
