module Chiasma.Ui.Measure(
  measureTree,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (zip)
import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)

import Chiasma.Data.Maybe (orElse)
import Chiasma.Ui.Data.Measure (MLayout(..), MPane(..), MeasureTree, MeasureTreeSub, Measured(..))
import Chiasma.Ui.Data.RenderableTree (RLayout(..), RPane(..), Renderable(..), RenderableNode, RenderableTree)
import Chiasma.Ui.Data.Tree (Tree(..))
import qualified Chiasma.Ui.Data.Tree as Tree (Node(..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(minSize, maxSize, fixedSize))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.Measure.Balance (balanceSizes)
import Chiasma.Ui.Measure.Weights (viewWeights)

minimizedSizeOrDefault :: ViewGeometry -> Float
minimizedSizeOrDefault = fromMaybe 2 . minSize

effectiveFixedSize :: ViewState -> ViewGeometry -> Maybe Float
effectiveFixedSize (ViewState minimized) viewGeom =
  if minimized then Just (minimizedSizeOrDefault viewGeom) else fixedSize viewGeom

actualSize :: (ViewGeometry -> Maybe Float) ->  ViewState -> ViewGeometry -> Maybe Float
actualSize getter viewState viewGeom =
  orElse (getter viewGeom) (effectiveFixedSize viewState viewGeom)

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
    paneSpacers = int2Float (length views) - 1.0
    cells = total - paneSpacers
    sizesInCells s = if s > 1 then s else s * cells
    minSizes = fmap sizesInCells (actualMinSizes measureData)
    maxSizes = fmap (fmap sizesInCells) (actualMaxSizes measureData)
    minimized = fmap (uncurry isMinimized) measureData
    weights = viewWeights measureData

measureSub :: Int -> Int -> Bool -> RenderableNode -> Int -> MeasureTreeSub
measureSub width height vertical (Tree.Sub tree) size =
  Tree.Sub $ measureLayout tree newWidth newHeight vertical
  where
    (newWidth, newHeight) = if vertical then (width, size) else (size, height)
measureSub _ _ vertical (Tree.Leaf (Renderable _ _ (RPane paneId top left))) size =
  Tree.Leaf (Measured size (MPane paneId (if vertical then top else left)))

measureLayout :: RenderableTree -> Int -> Int -> Bool -> MeasureTree
measureLayout (Tree (Renderable _ _ (RLayout (RPane refId refTop refLeft) vertical)) sub) width height parentVertical =
  Tree (Measured sizeInParent (MLayout refId (if parentVertical then refTop else refLeft) vertical)) measuredSub
  where
    sizeInParent = if parentVertical then height else width
    subTotalSize = if vertical then height else width
    sizes = measureLayoutViews (int2Float subTotalSize) sub
    measuredSub = uncurry (measureSub width height vertical) <$> NonEmpty.zip sub sizes

measureTree :: RenderableTree -> Int -> Int -> MeasureTree
measureTree tree width height =
  measureLayout tree width height False
