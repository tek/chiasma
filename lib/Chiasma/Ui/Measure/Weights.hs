module Chiasma.Ui.Measure.Weights(
  viewWeights,
  normalizeWeights,
  amendAndNormalizeWeights,
) where

import GHC.Float (int2Float)
import Data.Maybe (fromMaybe, isJust, catMaybes, isNothing)
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))

effectiveWeight :: ViewState -> ViewGeometry -> Maybe Float
effectiveWeight (ViewState minimized) (ViewGeometry _ _ fixedSize _ weight _) =
  if isJust fixedSize || minimized then Just 0 else weight

amendWeights :: [Maybe Float] -> [Float]
amendWeights weights =
  fmap (fromMaybe emptyWeight) weights
  where
    total = sum (catMaybes weights)
    normTotal = if total == 0 then 1 else total
    empties = length (filter isNothing weights)
    normEmpties = if empties == 0 then 1 else empties
    emptyWeight = normTotal / int2Float normEmpties

normalizeWeights :: [Float] -> [Float]
normalizeWeights weights =
  fmap (/ normTotal) weights
  where
    total = sum weights
    normTotal = if total == 0 then 1 else total

amendAndNormalizeWeights :: [Maybe Float] -> [Float]
amendAndNormalizeWeights = normalizeWeights . amendWeights

viewWeights :: [(ViewState, ViewGeometry)] -> [Float]
viewWeights = amendAndNormalizeWeights . fmap (uncurry effectiveWeight)
