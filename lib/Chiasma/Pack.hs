module Chiasma.Pack(
  packWindow,
) where

import Control.Lens (each, mapMOf_)
import Control.Monad (when)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (reverse, sortWith, toList)
import qualified Data.Set as Set (fromList, size)
import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Doc, line, pretty, (<+>), (<>))

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Command.Pane (movePane, resizePane)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.Text.Pretty (prettyS)
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
describeVertical True = prettyS "vertically"
describeVertical False = prettyS "horizontally"

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
  viewsLog $ prettyS "resizing pane" <+> pretty paneId <+> prettyS "to" <+> pretty size <+> describeVertical vertical
  resizePane paneId vertical size

needPositioning ::
  MonadDeepState s Views m =>
  NonEmpty MeasureTreeSub ->
  m Bool
needPositioning sub = do
  viewsLog $ "-----------" <+> pretty sub
  viewsLog $ "-----------" <+> pretty (NonEmpty.sortWith position sub)
  return $ wrongOrder || wrongDirection
  where
    wrongOrder =
      sort positions /= positions
    wrongDirection =
      Set.size (Set.fromList positions) /= (length positions)
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
      need <- needPos
      when need runPos
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
  viewsLog $ pretty (T.pack "measured tree:") <> line <> pretty measures
  packTree measures
