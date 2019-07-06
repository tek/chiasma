module Chiasma.Pack where

import Control.Lens (each, mapMOf_)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (reverse, toList)
import qualified Data.Set as Set (fromList, size)
import Data.Text.Prettyprint.Doc (Doc, line, pretty, (<+>), (<>))

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Command.Pane (movePane, resizePane)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState(..))
import Chiasma.Ui.Data.Measure (MLayout(..), MPane(..), MeasureTree, MeasureTreeSub, Measured(Measured))
import Chiasma.Ui.Data.Tree (Node(Sub, Leaf), Tree(Tree))
import qualified Chiasma.Ui.Data.Tree as Tree (subTree)
import Chiasma.Ui.Measure (measureTree)
import Chiasma.View (viewsLog)

packPane ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  PaneId ->
  Bool ->
  PaneId ->
  m ()
packPane refId vertical paneId =
  when (paneId /= refId) $ movePane paneId refId vertical

positionView ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Bool ->
  PaneId ->
  MeasureTreeSub ->
  m ()
positionView vertical refId =
  position
  where
    position (Sub (Tree (Measured _ (MLayout layoutRef _ _)) _)) =
      packPane refId vertical layoutRef
    position (Leaf (Measured _ (MPane paneId _))) =
      packPane refId vertical paneId

describeVertical :: Bool -> Doc a
describeVertical True = "vertically"
describeVertical False = "horizontally"

resizeView ::
  MonadDeepState s Views m =>
  MonadFree TmuxThunk m =>
  MonadError RenderError m =>
  Bool ->
  MeasureTreeSub ->
  m ()
resizeView vertical (Sub (Tree (Measured size (MLayout refId _ _)) _)) = do
  viewsLog $ "resizing layout with ref" <+> pretty refId <+> "to" <+> pretty size <+> describeVertical vertical
  resizePane refId vertical size
resizeView vertical (Leaf (Measured size (MPane paneId _))) = do
  viewsLog $ "resizing pane" <+> pretty paneId <+> "to" <+> pretty size <+> describeVertical vertical
  resizePane paneId vertical size

needPositioning ::
  NonEmpty MeasureTreeSub ->
  Bool
needPositioning sub =
  wrongOrder || wrongDirection
  where
    wrongOrder =
      sort positions /= positions
    wrongDirection =
      Set.size (Set.fromList positions) /= length positions
    positions =
      NonEmpty.toList $ position <$> sub
    position (Sub (Tree (Measured _ (MLayout _ pos _)) _)) =
      pos
    position (Leaf (Measured _ (MPane _ pos))) =
      pos

packTree ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  MeasureTree ->
  m ()
packTree =
  pack
  where
    pack (Tree (Measured _ (MLayout ref _ vertical)) sub) = do
      when needPos runPos
      mapMOf_ (each . Tree.subTree) pack sub
      traverse_ (resizeView vertical) sub
      where
        needPos = needPositioning sub
        runPos = do
          viewsLog $ "repositioning views" <+> pretty sub
          traverse_ (positionView vertical ref) (NonEmpty.reverse sub)

packWindow ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  WindowState ->
  m ()
packWindow (WindowState (Codec.Window _ width height) _ _ tree _) = do
  let measures = measureTree tree width height
  viewsLog $ "measured tree:" <> line <> pretty measures
  packTree measures
