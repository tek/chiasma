{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Window where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty (head, nonEmpty)
import Data.Text.Prettyprint.Doc (line, pretty, vsep, (<+>), (<>))

import qualified Chiasma.Codec.Data as Codec (Pane(Pane, paneId), Window(Window, windowId))
import qualified Chiasma.Codec.Data.PaneDetail as Codec (PaneDetail(PaneDetail))
import qualified Chiasma.Codec.Data.PaneDetail as PaneDetail (PaneDetail(..))
import qualified Chiasma.Command.Pane as Cmd (closePane, firstWindowPane, windowPanesAs)
import qualified Chiasma.Command.Window as Cmd (newWindow, splitWindowAs, window)
import Chiasma.Data.Ident (Ident, identText, identify)
import Chiasma.Data.Maybe (findMaybe, maybeExcept, orElse)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(NoPrincipal))
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState(..))
import Chiasma.Pane (addPane)
import Chiasma.Ui.Data.RenderableTree (
  RLayout(..),
  RPane(..),
  Renderable(..),
  RenderableNode,
  RenderableTree,
  )
import qualified Chiasma.Ui.Data.Tree as Tree (Node(Sub, Leaf), Tree(Tree))
import Chiasma.Ui.Data.View (Tree(..), TreeSub(..), ViewTree, ViewTreeSub)
import qualified Chiasma.Ui.Data.View as Ui (Layout(..), Pane(Pane), PaneView, View(View))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry, position))
import Chiasma.Ui.Data.ViewState (ViewState)
import Chiasma.View (findOrCreateView, viewsLog, viewsLogS)
import qualified Chiasma.View as Views (insertPane, insertWindow, pane, paneById, updatePane, updateWindow, window)

findOrCreateWindow ::
  (MonadDeepState s Views m, MonadError RenderError m) =>
  Ident ->
  m (Tmux.View WindowId)
findOrCreateWindow =
  findOrCreateView Views.window Views.insertWindow

registerWindowId ::
  (MonadDeepState s Views m) =>
  Ident ->
  WindowId ->
  m ()
registerWindowId ident windowId =
  modify $ Views.updateWindow $ Tmux.View ident (Just windowId)

spawnWindow ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  SessionId ->
  Ident ->
  m Codec.Window
spawnWindow sid ident = do
  win@(Codec.Window windowId _ _) <- Cmd.newWindow sid ident
  registerWindowId ident windowId
  viewsLogS $ "spawned window in session " <> show sid <> " with id " <> show windowId
  return win

findPrincipalSub :: ViewTreeSub -> Maybe Ui.PaneView
findPrincipalSub (TreeNode t) = findPrincipal t
findPrincipalSub (TreeLeaf p@(Ui.View _ _ _ (Ui.Pane True _ _))) = Just p
findPrincipalSub _ = Nothing

findPrincipal :: ViewTree -> Maybe Ui.PaneView
findPrincipal (Tree _ sub) =
  findMaybe findPrincipalSub sub

principalPane ::
  (MonadDeepState s Views m, MonadError RenderError m) =>
  ViewTree ->
  m (Ui.PaneView, Tmux.View PaneId)
principalPane tree = do
  uiPane@(Ui.View uiPaneIdent _ _ _) <- maybeExcept (RenderError.NoPrincipal $ identify tree) $ findPrincipal tree
  existingTmuxPane <- gets $ Views.pane uiPaneIdent
  tmuxPane <- either (const $ addPane uiPaneIdent) return existingTmuxPane
  return (uiPane, tmuxPane)

syncPrincipal ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  WindowId ->
  ViewTree ->
  m ()
syncPrincipal windowId tree@(Tree (Ui.View layoutIdent _ _ _) _) = do
  (Codec.Pane paneId _ _) <- Cmd.firstWindowPane windowId
  existing <- gets (Views.paneById paneId)
  case existing of
    Nothing -> do
      (_, Tmux.View paneIdent _) <- principalPane tree
      viewsLog $ "setting principal of layout" <+> pretty layoutIdent <+> " to pane " <+> pretty paneIdent <+> "/" <+>
        pretty paneId
      modify $ Views.updatePane (Tmux.View paneIdent (Just paneId))
    _ -> return ()

ensureWindow ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  SessionId ->
  Tmux.View WindowId ->
  Maybe WindowId ->
  ViewTree ->
  m Codec.Window
ensureWindow sid (Tmux.View ident mayWid) newSessionWid tree = do
  preexisting <- join <$> traverse Cmd.window (orElse newSessionWid mayWid)
  window <- maybe (spawnWindow sid ident) return preexisting
  syncPrincipal (Codec.windowId window) tree
  return window

findOrCreatePane :: MonadDeepState s Views m => Ident -> m (Tmux.View PaneId)
findOrCreatePane =
  findOrCreateView Views.pane Views.insertPane

nativePane ::
  MonadFree TmuxThunk m =>
  WindowId ->
  Tmux.View PaneId ->
  m (Maybe Codec.PaneDetail)
nativePane windowId (Tmux.View _ (Just paneId)) =
  find sameId <$> Cmd.windowPanesAs windowId
  where
    sameId (Codec.PaneDetail i _ _ _ _) = i == paneId
nativePane _ _ = return Nothing

openPane ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  WindowId ->
  m Codec.PaneDetail
openPane dir windowId = do
  detail <- Cmd.splitWindowAs dir windowId
  viewsLogS $ "opened pane " <> show (PaneDetail.paneId detail) <> " in window " <> show windowId
  return detail

ensurePaneOpen ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  Maybe Codec.PaneDetail ->
  WindowId ->
  m Codec.PaneDetail
ensurePaneOpen _ (Just detail) _ =
  return detail
ensurePaneOpen dir Nothing windowId =
  openPane dir windowId

ensurePaneClosed ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  Maybe Codec.PaneDetail ->
  m ()
ensurePaneClosed (Just (Codec.PaneDetail i _ _ _ _)) = do
  viewsLogS $ "closing pane " <> show i
  Cmd.closePane i
ensurePaneClosed _ = return ()

ensurePane ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  WindowId ->
  Ui.PaneView ->
  m (Maybe RenderableNode)
ensurePane cwd windowId (Ui.View paneIdent vState geometry (Ui.Pane open _ customDir)) = do
  tmuxPane <- findOrCreatePane paneIdent
  existingPane <- nativePane windowId tmuxPane
  updatedPane <-
    if open then Just <$> ensurePaneOpen dir existingPane windowId
    else Nothing <$ ensurePaneClosed existingPane
  modify $ Views.updatePane (Tmux.View paneIdent (PaneDetail.paneId <$> updatedPane))
  return $ cons <$> updatedPane
  where
    dir = fromMaybe cwd customDir
    cons (Codec.PaneDetail i _ _ top left) =
      Tree.Leaf . Renderable vState geometry $ RPane i top left

refPane :: RenderableNode -> RPane
refPane (Tree.Sub (Tree.Tree (Renderable _ _ (RLayout ref _)) _)) = ref
refPane (Tree.Leaf (Renderable _ _ pane)) = pane

renderableTree ::
  ViewState ->
  ViewGeometry ->
  Bool ->
  [RenderableNode] ->
  Maybe RenderableTree
renderableTree vState geometry vertical sub = do
  sub' <- NonEmpty.nonEmpty sub
  return $ Tree.Tree (Renderable vState geometry (RLayout (refPane $ NonEmpty.head sub') vertical)) sub'

viewPosition :: ViewTreeSub -> Float
viewPosition (TreeNode (Tree (Ui.View _ _ ViewGeometry { position = pos } _) _)) =
  fromMaybe 0.5 pos
viewPosition (TreeLeaf (Ui.View _ _ ViewGeometry { position = pos } _)) =
  fromMaybe 0.5 pos

ensureView ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  WindowId ->
  ViewTree ->
  m (Maybe RenderableTree)
ensureView cwd windowId =
  ensureTree
  where
    ensureTree (Tree (Ui.View layoutIdent vState geometry (Ui.Layout vertical)) sub) = do
      ensuredSub <- traverse ensureNode sortedSub
      viewsLog $ pretty ("new sub for layout `" <> identText layoutIdent <> "`:") <> line <>
        vsep (pretty <$> ensuredSub)
      return $ renderableTree vState geometry vertical $ catMaybes ensuredSub
      where
        sortedSub = sortOn viewPosition sub
    ensureNode (TreeNode t) = do
      newTree <- ensureTree t
      return $ Tree.Sub <$> newTree
    ensureNode (TreeLeaf v) =
      ensurePane cwd windowId v

windowState ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Ident ->
  Codec.Window ->
  RenderableTree ->
  m WindowState
windowState windowIdent window tree = do
  nativeRef <- Cmd.firstWindowPane (Codec.windowId window)
  return $ WindowState window nativeRef windowIdent tree (Codec.paneId nativeRef)
