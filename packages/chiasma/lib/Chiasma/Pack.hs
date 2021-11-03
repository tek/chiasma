module Chiasma.Pack where

import Control.Lens (each, mapMOf_)
import qualified Data.List.NonEmpty as NonEmpty (reverse, toList)
import qualified Data.Set as Set (fromList, size)
import Prettyprinter (Doc, line, pretty, (<+>))

import qualified Chiasma.Codec.Data.Window as Codec (Window (Window))
import Chiasma.Command.Pane (movePane, resizePane)
import Chiasma.Data.Axis (Axis (Horizontal, Vertical))
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views)
import Chiasma.Data.WindowState (WindowState (..))
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Ui.Data.Measure (MLayout (..), MPane (..), MeasureTree, MeasureTreeSub, Measured (Measured))
import Chiasma.Ui.Data.Tree (Node (Leaf, Sub), Tree (Tree))
import qualified Chiasma.Ui.Data.Tree as Tree (subTree)
import Chiasma.Ui.Measure (measureTree)
import Chiasma.View (viewsLog)

packPane ::
  Members [AtomicState Views, Tmux] r =>
  PaneId ->
  Axis ->
  PaneId ->
  Sem r ()
packPane refId axis paneId =
  when (paneId /= refId) do
    movePane paneId refId axis

positionView ::
  Members [AtomicState Views, Tmux] r =>
  Axis ->
  PaneId ->
  MeasureTreeSub ->
  Sem r ()
positionView axis refId =
  position
  where
    position (Sub (Tree (Measured _ (MLayout layoutRef _ _ _)) _)) =
      packPane refId axis layoutRef
    position (Leaf (Measured _ (MPane paneId _ _))) =
      packPane refId axis paneId

describeAxis :: Axis -> Doc a
describeAxis = \case
  Vertical -> "vertically"
  Horizontal -> "horizontally"

resizeView ::
  Members [AtomicState Views, Tmux] r =>
  Axis ->
  MeasureTreeSub ->
  Sem r ()
resizeView axis = \case
  Sub (Tree (Measured size (MLayout refId _ _ _)) _) -> do
    viewsLog ("resizing layout with ref" <+> pretty refId <+> "to" <+> pretty size <+> describeAxis axis)
    resizePane refId axis size
  Leaf (Measured size (MPane paneId _ _)) -> do
    viewsLog $ "resizing pane" <+> pretty paneId <+> "to" <+> pretty size <+> describeAxis axis
    resizePane paneId axis size

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
  Members [AtomicState Views, Tmux] r =>
  MeasureTree ->
  Sem r ()
packTree =
  pack
  where
    pack (Tree (Measured _ (MLayout ref _ _ axis)) sub) = do
      when needPos runPos
      mapMOf_ (each . Tree.subTree) pack sub
      traverse_ (resizeView axis) sub
      where
        needPos = needPositioning sub
        runPos = do
          viewsLog $ "repositioning views" <+> pretty sub
          traverse_ (positionView axis ref) (NonEmpty.reverse sub)

packWindow ::
  Members [AtomicState Views, Tmux] r =>
  WindowState ->
  Sem r ()
packWindow (WindowState (Codec.Window _ width height) _ _ tree _) = do
  let measures = measureTree tree width height
  viewsLog $ "measured tree:" <> line <> pretty measures
  packTree measures
