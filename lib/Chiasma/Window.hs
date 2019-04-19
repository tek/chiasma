{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Window where

import Control.Monad (join)
import Control.Monad.DeepState (MonadDeepState, gets, modify)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Data.Foldable (find)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty (head, nonEmpty)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (line, pretty, vsep, (<>))

import qualified Chiasma.Codec.Data as Codec (Pane(Pane, paneId), Window(Window, windowId))
import qualified Chiasma.Command.Pane as Cmd (closePane, firstWindowPane, windowPanes)
import qualified Chiasma.Command.Window as Cmd (newWindow, splitWindow, window)
import Chiasma.Data.Ident (Ident, identString, identify)
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
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
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
  viewsLogS $ "spawned window in session " ++ show sid ++ " with id " ++ show windowId
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
      viewsLogS $ "setting principal of layout " ++ identString layoutIdent ++ " to pane " ++ identString paneIdent ++
        "/" ++ show paneId
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
  m (Maybe Codec.Pane)
nativePane windowId (Tmux.View _ (Just paneId)) =
  find sameId <$> Cmd.windowPanes windowId
  where
    sameId (Codec.Pane i _ _) = i == paneId
nativePane _ _ = return Nothing

openPane ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  WindowId ->
  m PaneId
openPane dir windowId = do
  (Codec.Pane i _ _) <- Cmd.splitWindow dir windowId
  viewsLogS $ "opened pane " ++ show i ++ " in window " ++ show windowId
  return i

ensurePaneOpen ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  Maybe Codec.Pane ->
  WindowId ->
  m PaneId
ensurePaneOpen dir existing windowId =
  maybe (openPane dir windowId) (return . Codec.paneId) existing

ensurePaneClosed ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  Maybe Codec.Pane ->
  m ()
ensurePaneClosed (Just (Codec.Pane i _ _)) = do
  viewsLogS $ "closing pane " ++ show i
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
  let dir = fromMaybe cwd customDir
  updatedId <-
    if open then Just <$> ensurePaneOpen dir existingPane windowId
    else Nothing <$ ensurePaneClosed existingPane
  modify $ Views.updatePane (Tmux.View paneIdent updatedId)
  return $ Tree.Leaf . Renderable vState geometry . RPane <$> updatedId

refPaneId :: RenderableNode -> PaneId
refPaneId (Tree.Sub (Tree.Tree (Renderable _ _ (RLayout refId _)) _)) = refId
refPaneId (Tree.Leaf (Renderable _ _ (RPane paneId))) = paneId

renderableTree ::
  ViewState ->
  ViewGeometry ->
  Bool ->
  [RenderableNode] ->
  Maybe RenderableTree
renderableTree vState geometry vertical sub = do
  sub' <- NonEmpty.nonEmpty sub
  let refId = refPaneId $ NonEmpty.head sub'
  return $ Tree.Tree (Renderable vState geometry (RLayout refId vertical)) sub'

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
      viewsLog $ pretty (T.pack $ "new sub for layout `" ++ identString layoutIdent ++ "`:") <> line <>
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
