module Chiasma.Pack(
  packWindow,
) where

import Control.Monad (filterM, when)
import Control.Monad.Error.Class (MonadError(throwError), liftEither)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState, gets)
-- import Control.Lens (traverseOf, each)
import Control.Lens
import Data.Either.Combinators (maybeToRight)
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Command.Pane (isPaneOpen, isPaneIdOpen, movePane, resizePane)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.List (head')
import Chiasma.Data.Maybe (maybeExcept)
import Chiasma.Data.PackError (PackError(PackError))
import Chiasma.Data.TmuxId (WindowId, PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState(..))
import qualified Chiasma.Data.WindowState as WindowState (WindowStateType(..))
import Chiasma.Lens.Tree (leafByIdent)
import Chiasma.Ui.Data.Measure (Measured(Measured), MeasureTree, _measuredView)
import Chiasma.Ui.Data.View (Tree(Tree), ViewTree, _subTree, TreeSub(..))
import qualified Chiasma.Ui.Data.View as Ui (
  View(View),
  PaneView,
  LayoutView,
  Layout(Layout),
  Pane(Pane),
  viewIdent,
  _leafData,
  )
import Chiasma.Ui.Measure (measureTree)
import qualified Chiasma.View as Views (pane)

refPaneError :: Ident -> PackError
refPaneError ident = PackError $ "reference pane `" ++ show ident ++ "` not found"

referenceUiPane :: Ident -> ViewTree -> Either PackError Ui.PaneView
referenceUiPane ident = maybeToRight (refPaneError ident) . leafByIdent ident

isTmuxPaneOpen ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  Ui.PaneView ->
  m Bool
isTmuxPaneOpen uiPane = do
  tmuxPane <- gets $ Views.pane (Ui.viewIdent uiPane)
  either (return . const False) isPaneOpen tmuxPane

layoutPanes :: [TreeSub (Measured Ui.LayoutView) (Measured Ui.PaneView)] -> [Ui.PaneView]
layoutPanes = toListOf (each . Ui._leafData . _measuredView)

referencePane ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  [TreeSub (Measured Ui.LayoutView) (Measured Ui.PaneView)] ->
  m (Maybe Ui.PaneView)
referencePane subs = do
  let panes = layoutPanes subs
  openPanes <- filterM isTmuxPaneOpen panes
  return $ head' openPanes

paneIdFatal :: (MonadState Views m, MonadError PackError m) => Ident -> m PaneId
paneIdFatal ident = do
  pane <- gets $ Views.pane ident
  case pane of
    (Right (Tmux.View _ (Just paneId))) -> return paneId
    _ -> throwError $ PackError $ "no tmux pane for `" ++ show ident ++ "`"

moveTmuxPane ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  Ident -> Ident -> Bool -> m ()
moveTmuxPane paneIdent refIdent vertical = do
  refId <- paneIdFatal refIdent
  paneId <- paneIdFatal paneIdent
  open <- isPaneIdOpen paneId
  when open $ movePane paneId refId vertical

packPane ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  Ui.PaneView ->
  Bool ->
  Ui.PaneView ->
  m ()
packPane refPane vertical pane@(Ui.View _ _ _ (Ui.Pane open _ _)) =
  when (open && pane /= refPane) $ moveTmuxPane (Ui.viewIdent pane) (Ui.viewIdent refPane) vertical

positionView ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  Bool ->
  Ui.PaneView ->
  TreeSub (Measured Ui.LayoutView) (Measured Ui.PaneView) ->
  m ()
positionView vertical refPane =
  position
  where
    pp = packPane refPane vertical
    position (TreeNode (Tree _ sub)) =
      traverse_ pp firstPane
      where
        firstPane = head' $ layoutPanes sub
    position (TreeLeaf (Measured pane _)) =
      pp pane

resizeViewWith ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  Int ->
  Ui.PaneView ->
  Bool ->
  m ()
resizeViewWith size pane vertical = do
  paneId <- paneIdFatal (Ui.viewIdent pane)
  resizePane paneId vertical size

resizeView ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  Bool ->
  TreeSub (Measured Ui.LayoutView) (Measured Ui.PaneView) ->
  m ()
resizeView vertical (TreeNode (Tree (Measured _ size) sub)) = do
  layoutRefPane <- referencePane sub
  ref <- maybeExcept (PackError "no ref pane") layoutRefPane
  resizeViewWith size ref vertical
resizeView vertical (TreeLeaf (Measured pane size)) =
  resizeViewWith size pane vertical

packTree ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  WindowId ->
  Ui.PaneView ->
  MeasureTree ->
  Ui.PaneView ->
  m ()
packTree _ _ measures initialRef =
  pack initialRef measures
  where
    pack ref (Tree (Measured (Ui.View _ _ _ (Ui.Layout vertical)) _) sub) = do
      layoutRefPane <- referencePane sub
      let newRefPane = fromMaybe ref layoutRefPane
      traverse_ (positionView vertical newRefPane) sub
      mapMOf_ (each . _subTree) (pack newRefPane) sub
      traverse_ (resizeView vertical) sub

packPristineWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  WindowState ->
  WindowId ->
  Ui.PaneView ->
  m ()
packPristineWindow = undefined

packTrackedWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  Tmux.View PaneId ->
  WindowState ->
  WindowId ->
  Ui.PaneView ->
  m ()
packTrackedWindow (Tmux.View paneIdent _) (WindowState (Codec.Window _ width height) _ _ tree _) windowId principal = do
  uiRef <- liftEither $ referenceUiPane paneIdent tree
  let measures = measureTree tree width height
  packTree windowId principal measures uiRef

packWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError PackError m) =>
  WindowState ->
  WindowId ->
  Ui.PaneView ->
  m ()
packWindow wState =
  pack (wsType wState) wState
  where
    pack WindowState.Pristine = packPristineWindow
    pack (WindowState.Tracked pane) = packTrackedWindow pane
