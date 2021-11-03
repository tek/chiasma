module Chiasma.Ui.Measure where

import qualified Data.List.NonEmpty as NonEmpty (zip)

import qualified Chiasma.Data.Axis as Axis
import Chiasma.Data.Axis (Axis (Horizontal, Vertical))
import Chiasma.Ui.Data.Measure (MLayout (..), MPane (..), MeasureTree, MeasureTreeSub, Measured (..))
import Chiasma.Ui.Data.RenderableTree (RLayout (..), RPane (..), Renderable (..), RenderableNode, RenderableTree)
import Chiasma.Ui.Data.Tree (Tree (..))
import qualified Chiasma.Ui.Data.Tree as Tree (Node (..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (fixedSize, maxSize, minSize))
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))
import Chiasma.Ui.Measure.Balance (balanceSizes)
import Chiasma.Ui.Measure.Weights (viewWeights)

minimizedSizeOrDefault :: ViewGeometry -> Float
minimizedSizeOrDefault = fromMaybe 2 . minSize

effectiveFixedSize :: ViewState -> ViewGeometry -> Maybe Float
effectiveFixedSize (ViewState minimized) viewGeom =
  if minimized then Just (minimizedSizeOrDefault viewGeom) else fixedSize viewGeom

actualSize :: (ViewGeometry -> Maybe Float) ->  ViewState -> ViewGeometry -> Maybe Float
actualSize getter viewState viewGeom =
  getter viewGeom <|> effectiveFixedSize viewState viewGeom

actualMinSizes :: NonEmpty (ViewState, ViewGeometry) -> NonEmpty Float
actualMinSizes =
  fmap (fromMaybe 0.0 . uncurry (actualSize minSize))

actualMaxSizes :: NonEmpty (ViewState, ViewGeometry) -> NonEmpty (Maybe Float)
actualMaxSizes =
  fmap (uncurry $ actualSize maxSize)

isMinimized :: ViewState -> ViewGeometry -> Bool
isMinimized (ViewState minimized) _ = minimized

subMeasureData :: RenderableNode -> (ViewState, ViewGeometry)
subMeasureData (Tree.Sub (Tree (Renderable s g _) _)) = (s, g)
subMeasureData (Tree.Leaf (Renderable s g _)) = (s, g)

measureLayoutViews :: Float -> NonEmpty RenderableNode -> NonEmpty Int
measureLayoutViews total views =
  balanceSizes minSizes maxSizes weights minimized cells
  where
    measureData = fmap subMeasureData views
    paneSpacers = fromIntegral (length views) - 1.0
    cells = total - paneSpacers
    sizesInCells s = if s > 1 then s else s * cells
    minSizes = fmap sizesInCells (actualMinSizes measureData)
    maxSizes = fmap (fmap sizesInCells) (actualMaxSizes measureData)
    minimized = fmap (uncurry isMinimized) measureData
    weights = viewWeights measureData

measureSub :: Int -> Int -> Axis -> RenderableNode -> Int -> MeasureTreeSub
measureSub width height axis (Tree.Sub tree) size =
  Tree.Sub (measureLayout tree newWidth newHeight axis)
  where
    (newWidth, newHeight) =
      case axis of
        Vertical -> (width, size)
        Horizontal -> (size, height)
measureSub _ _ (Axis.vertical -> vertical) (Tree.Leaf (Renderable _ _ (RPane paneId top left))) size =
  Tree.Leaf (Measured size (MPane paneId (if vertical then top else left) (if vertical then left else top)))

measureLayout :: RenderableTree -> Int -> Int -> Axis -> MeasureTree
measureLayout (Tree (Renderable _ _ (RLayout (RPane refId refTop refLeft) axis)) sub) width height (Axis.vertical -> parentVertical) =
  Tree (Measured sizeInParent (MLayout refId mainPos offPos axis)) measuredSub
  where
    sizeInParent = if parentVertical then height else width
    mainPos = if parentVertical then refTop else refLeft
    offPos = if parentVertical then refLeft else refTop
    subTotalSize = if Axis.vertical axis then height else width
    sizes = measureLayoutViews (fromIntegral subTotalSize) sub
    measuredSub = uncurry (measureSub width height axis) <$> NonEmpty.zip sub sizes

measureTree :: RenderableTree -> Int -> Int -> MeasureTree
measureTree tree width height =
  measureLayout tree width height Horizontal
