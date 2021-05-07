module Chiasma.Pack where

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Command.Pane (movePane, resizePane)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState(..))
import Chiasma.Ui.Data.Measure (MLayout(..), MPane(..), MeasureTree, MeasureTreeSub, Measured(Measured))
import Chiasma.Ui.Data.Tree (Node(Sub, Leaf), Tree(Tree))
import qualified Chiasma.Ui.Data.Tree as Tree (subTree)
import Chiasma.Ui.Measure (measureTree)
import Chiasma.View (viewsLog)
import Control.Lens (each, mapMOf_)
import Control.Monad.Free.Class (MonadFree)
import qualified Data.List.NonEmpty as NonEmpty (reverse, toList)
import qualified Data.Set as Set (fromList, size)
import Data.Text.Prettyprint.Doc (Doc, line, pretty, (<+>))

packPane ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  PaneId ->
  Bool ->
  PaneId ->
  m ()
packPane refId vertical paneId =
  when (paneId /= refId) $ movePane paneId refId vertical

positionView ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  Bool ->
  PaneId ->
  MeasureTreeSub ->
  m ()
positionView vertical refId =
  position
  where
    position (Sub (Tree (Measured _ (MLayout layoutRef _ _ _)) _)) =
      packPane refId vertical layoutRef
    position (Leaf (Measured _ (MPane paneId _ _))) =
      packPane refId vertical paneId

describeVertical :: Bool -> Doc a
describeVertical True = "vertically"
describeVertical False = "horizontally"

resizeView ::
  MonadDeepState s Views m =>
  MonadFree TmuxThunk m =>
  Bool ->
  MeasureTreeSub ->
  m ()
resizeView vertical (Sub (Tree (Measured size (MLayout refId _ _ _)) _)) = do
  viewsLog $ "resizing layout with ref" <+> pretty refId <+> "to" <+> pretty size <+> describeVertical vertical
  resizePane refId vertical size
resizeView vertical (Leaf (Measured size (MPane paneId _ _))) = do
  viewsLog $ "resizing pane" <+> pretty paneId <+> "to" <+> pretty size <+> describeVertical vertical
  resizePane paneId vertical size

needPositioning ::
  NonEmpty MeasureTreeSub ->
  Bool
needPositioning sub =
  wrongOrder || wrongDirection || unaligned
  where
    wrongOrder =
      sort positions /= positions
    wrongDirection =
      Set.size (Set.fromList positions) /= length positions
    unaligned =
      length sub > 1 && Set.size (Set.fromList offPositions) > 1
    positions =
      NonEmpty.toList $ position <$> sub
    position (Sub (Tree (Measured _ (MLayout _ mainPos _ _)) _)) =
      mainPos
    position (Leaf (Measured _ (MPane _ mainPos _))) =
      mainPos
    offPositions =
      NonEmpty.toList $ offPosition <$> sub
    offPosition (Sub (Tree (Measured _ (MLayout _ _ offPos _)) _)) =
      offPos
    offPosition (Leaf (Measured _ (MPane _ _ offPos))) =
      offPos

packTree ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  MeasureTree ->
  m ()
packTree =
  pack
  where
    pack (Tree (Measured _ (MLayout ref _ _ vertical)) sub) = do
      when needPos runPos
      mapMOf_ (each . Tree.subTree) pack sub
      traverse_ (resizeView vertical) sub
      where
        needPos = needPositioning sub
        runPos = do
          viewsLog $ "repositioning views" <+> pretty sub
          traverse_ (positionView vertical ref) (NonEmpty.reverse sub)

packWindow ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  WindowState ->
  m ()
packWindow (WindowState (Codec.Window _ width height) _ _ tree _) = do
  let measures = measureTree tree width height
  viewsLog $ "measured tree:" <> line <> pretty measures
  packTree measures
