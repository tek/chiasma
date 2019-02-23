module Chiasma.Pack(
  packWindow,
) where

import Control.Lens (each, mapMOf_)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Data.Foldable (traverse_)
import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Doc, pretty, line, (<>), (<+>))

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Command.Pane (isPaneIdOpen, movePane, resizePane)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.Text.Pretty (prettyS)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState(..))
import Chiasma.Ui.Data.Measure (Measured(Measured), MeasureTree, MeasureTreeSub, MPane(..), MLayout(..))
import Chiasma.Ui.Data.Tree (Tree(Tree), Node(Sub, Leaf))
import qualified Chiasma.Ui.Data.Tree as Tree (subTree)
import Chiasma.Ui.Measure (measureTree)
import Chiasma.View (viewsLog)

moveTmuxPane ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  PaneId -> PaneId -> Bool -> m ()
moveTmuxPane paneId refId vertical = do
  open <- isPaneIdOpen paneId
  when open $ movePane paneId refId vertical

packPane ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  PaneId ->
  Bool ->
  PaneId ->
  m ()
packPane refId vertical paneId =
  when (paneId /= refId) $ moveTmuxPane paneId refId vertical

positionView ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Bool ->
  PaneId ->
  MeasureTreeSub ->
  m ()
positionView vertical refId =
  position
  where
    position (Sub (Tree (Measured _ (MLayout layoutRefId _)) _)) =
      packPane refId vertical layoutRefId
    position (Leaf (Measured _ (MPane paneId))) =
      packPane refId vertical paneId

describeVertical :: Bool -> Doc a
describeVertical True = prettyS "vertically"
describeVertical False = prettyS "horizontally"

resizeView ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Bool ->
  MeasureTreeSub ->
  m ()
resizeView vertical (Sub (Tree (Measured size (MLayout refId _)) _)) = do
  viewsLog $ prettyS "resizing layout with ref" <+> pretty refId <+> prettyS "to" <+> pretty size <+>
    describeVertical vertical
  resizePane refId vertical size
resizeView vertical (Leaf (Measured size (MPane paneId))) = do
  viewsLog $ prettyS "resizing pane" <+> pretty paneId <+> prettyS "to" <+> pretty size <+> describeVertical vertical
  resizePane paneId vertical size

packTree ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  MeasureTree ->
  m ()
packTree =
  pack
  where
    pack (Tree (Measured _ (MLayout ref vertical)) sub) = do
      traverse_ (positionView vertical ref) sub
      mapMOf_ (each . Tree.subTree) pack sub
      traverse_ (resizeView vertical) sub

packWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  WindowState ->
  m ()
packWindow (WindowState (Codec.Window _ width height) _ _ tree _) = do
  let measures = measureTree tree width height
  viewsLog $ pretty (T.pack "measured tree:") <> line <> pretty measures
  packTree measures
