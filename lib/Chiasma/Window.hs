{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Window(
  findOrCreateWindow,
  ensureWindow,
  principalPane,
  findPrincipal,
  ensureView,
  windowState,
) where

import Control.Monad (join)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState, modify, gets)
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Foldable (traverse_, find)
import Data.Maybe (fromMaybe)
import qualified Chiasma.Command.Window as Cmd (window, newWindow, splitWindow)
import qualified Chiasma.Command.Pane as Cmd (windowPanes, closePane, firstWindowPane)
import qualified Chiasma.Codec.Data as Codec (Window(Window, windowId), Pane(Pane, paneId))
import Chiasma.Data.TmuxId (SessionId, WindowId, PaneId)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Maybe (maybeExcept, findMaybe)
import Chiasma.Data.RenderError (RenderError(RenderError))
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState(..))
import qualified Chiasma.Data.WindowState as WindowStateType (WindowStateType(..))
import Chiasma.Pane (addPane)
import Chiasma.Ui.Data.View (ViewTree, Tree(..), ViewTreeSub, TreeSub(..))
import qualified Chiasma.Ui.Data.View as Ui (View(View), Pane(Pane), PaneView)
import Chiasma.View (findOrCreateView, paneById)
import qualified Chiasma.View as Views (window, insertWindow, updateWindow, pane, updatePane, insertPane)

findOrCreateWindow ::
  (MonadState Views m, MonadError RenderError m) =>
  Ident ->
  m (Tmux.View WindowId)
findOrCreateWindow = findOrCreateView Views.window Views.insertWindow

spawnWindow ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  SessionId ->
  Ident ->
  m Codec.Window
spawnWindow sid ident = do
  win@(Codec.Window windowId _ _) <- Cmd.newWindow sid ident
  modify $ Views.updateWindow $ Tmux.View ident (Just windowId)
  return win

findPrincipalSub :: ViewTreeSub -> Maybe Ui.PaneView
findPrincipalSub (TreeNode t) = findPrincipal t
findPrincipalSub (TreeLeaf p) = Just p

findPrincipal :: ViewTree -> Maybe Ui.PaneView
findPrincipal (Tree _ sub) =
  findMaybe findPrincipalSub sub

principalNative ::
  (MonadFree TmuxThunk m, MonadError RenderError m) =>
  WindowId ->
  m Codec.Pane
principalNative windowId = do
  panes <- Cmd.windowPanes windowId
  case panes of
    (princ : _) -> return princ
    _ -> throwError $ RenderError $ "tmux returned no panes for window `" ++ show windowId ++ "`"

principalPane ::
  (MonadState Views m, MonadError RenderError m) =>
  ViewTree ->
  m (Ui.PaneView, Tmux.View PaneId)
principalPane tree = do
  uiPane@(Ui.View uiPaneIdent _ _ _) <- maybeExcept (RenderError "no principal ui pane in layout") $ findPrincipal tree
  existingTmuxPane <- gets $ Views.pane uiPaneIdent
  tmuxPane <- either (const $ addPane uiPaneIdent) return existingTmuxPane
  return (uiPane, tmuxPane)

syncPrincipal ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  WindowId ->
  ViewTree ->
  m ()
syncPrincipal windowId tree = do
  (_, Tmux.View paneIdent _) <- principalPane tree
  (Codec.Pane paneId _ _) <- principalNative windowId
  modify $ Views.updatePane (Tmux.View paneIdent (Just paneId))

ensureWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  SessionId ->
  Tmux.View WindowId ->
  ViewTree ->
  m Codec.Window
ensureWindow sid (Tmux.View ident mayWid) tree = do
  preexisting <- join <$> traverse Cmd.window mayWid
  window <- maybe (spawnWindow sid ident) return preexisting
  syncPrincipal (Codec.windowId window) tree
  return window

findOrCreatePane :: MonadState Views m => Ident -> m (Tmux.View PaneId)
findOrCreatePane =
  findOrCreateView Views.pane Views.insertPane

nativePane ::
  MonadFree TmuxThunk m =>
  WindowId ->
  Tmux.View PaneId ->
  m (Maybe Codec.Pane)
nativePane windowId (Tmux.View _ (Just paneId)) = do
  wps <- Cmd.windowPanes windowId
  return $ find sameId wps
  where
    sameId (Codec.Pane i _ _) = i == paneId
nativePane _ _ = return Nothing

ensurePaneOpen ::
  (MonadFree TmuxThunk m) =>
  FilePath ->
  Maybe Codec.Pane ->
  WindowId ->
  m PaneId
ensurePaneOpen dir existing windowId = do
  (Codec.Pane i _ _) <- maybe (Cmd.splitWindow dir windowId) return existing
  return i

ensurePaneClosed ::
  (MonadFree TmuxThunk m) =>
  Maybe Codec.Pane ->
  m ()
ensurePaneClosed (Just (Codec.Pane i _ _)) = Cmd.closePane i
ensurePaneClosed _ = return ()

ensurePane ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  WindowId ->
  Ui.PaneView ->
  m ()
ensurePane cwd windowId (Ui.View paneIdent _ _ (Ui.Pane open _ customDir)) = do
  tmuxPane <- findOrCreatePane paneIdent
  existingPane <- nativePane windowId tmuxPane
  let dir = fromMaybe cwd customDir
  updatedId <-
    if open then Just <$> ensurePaneOpen dir existingPane windowId
    else Nothing <$ ensurePaneClosed existingPane
  modify $ Views.updatePane (Tmux.View paneIdent updatedId)

ensureView ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  WindowId ->
  ViewTree ->
  m ()
ensureView cwd windowId =
  ensureTree
  where
    ensureTree (Tree _ sub) = traverse_ ensureNode sub
    ensureNode (TreeNode t) = ensureTree t
    ensureNode (TreeLeaf v) = ensurePane cwd windowId v

windowState ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Ident ->
  Codec.Window ->
  ViewTree ->
  m WindowState
windowState windowIdent window tree = do
  nativeRef <- Cmd.firstWindowPane (Codec.windowId window)
  ref <- gets $ paneById (Codec.paneId nativeRef)
  let tpe = maybe WindowStateType.Pristine WindowStateType.Tracked ref
  return $ WindowState window nativeRef windowIdent tree tpe
