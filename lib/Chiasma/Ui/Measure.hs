module Chiasma.Ui.Measure(
  measureTree,
) where

import GHC.Float (int2Float)
import Data.Maybe (fromMaybe)
import Chiasma.Ui.Data.Measure (MeasureTree, Measured(..), MeasureTreeSub)
import Chiasma.Ui.Data.View (ViewTree, Tree(..), View(View), Layout(Layout), TreeSub(..), ViewTreeSub, Pane(Pane))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(minSize, maxSize, fixedSize))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.Measure.Weights (viewWeights)
import Chiasma.Ui.Measure.Balance (balanceSizes)

isViewOpen :: ViewTreeSub -> Bool
isViewOpen (TreeNode (Tree _ sub)) =
  any isViewOpen sub
isViewOpen (TreeLeaf (View _ _ _ (Pane open _ _))) =
  open

minimizedSizeOrDefault :: ViewGeometry -> Float
minimizedSizeOrDefault = fromMaybe 2 . minSize

effectiveFixedSize :: ViewState -> ViewGeometry -> Maybe Float
effectiveFixedSize (ViewState minimized) viewGeom =
  if minimized then Just (minimizedSizeOrDefault viewGeom) else fixedSize viewGeom

orElse :: Maybe a -> Maybe a -> Maybe a
orElse _ (Just a) = Just a
orElse fallback Nothing = fallback

actualSize :: (ViewGeometry -> Maybe Float) ->  ViewState -> ViewGeometry -> Maybe Float
actualSize getter viewState viewGeom =
  orElse (getter viewGeom) (effectiveFixedSize viewState viewGeom)

actualMinSizes :: [(ViewState, ViewGeometry)] -> [Float]
actualMinSizes =
  fmap (fromMaybe 0.0 . uncurry (actualSize minSize))

actualMaxSizes :: [(ViewState, ViewGeometry)] -> [Maybe Float]
actualMaxSizes =
  fmap (uncurry $ actualSize maxSize)

isMinimized :: ViewState -> ViewGeometry -> Bool
isMinimized (ViewState minimized) _ = minimized

subMeasureData :: ViewTreeSub -> (ViewState, ViewGeometry)
subMeasureData (TreeNode (Tree (View _ s g _) _)) = (s, g)
subMeasureData (TreeLeaf (View _ s g _)) = (s, g)

measureLayoutViews :: Float -> [ViewTreeSub] -> [Float]
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

measureSub :: Float -> Float -> Bool -> ViewTreeSub -> Float -> MeasureTreeSub
measureSub width height vertical (TreeNode tree) size =
  TreeNode $ measureLayout tree newWidth newHeight
  where
    (newWidth, newHeight) = if vertical then (width, size) else (size, height)
measureSub _ _ _ (TreeLeaf v) size =
  TreeLeaf (Measured v (round size))

measureLayout :: ViewTree -> Float -> Float -> MeasureTree
measureLayout (Tree v@(View _ _ _ (Layout vertical)) sub) width height =
  Tree (Measured v (round size)) measuredSub
  where
    size = if vertical then height else width
    views = filter isViewOpen sub
    sizes = measureLayoutViews size views
    measuredSub = uncurry (measureSub width height vertical) <$> zip views sizes

measureTree :: ViewTree -> Int -> Int -> MeasureTree
measureTree tree width height =
  measureLayout tree (int2Float width) (int2Float height)
