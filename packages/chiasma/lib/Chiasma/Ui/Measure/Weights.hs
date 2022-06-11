module Chiasma.Ui.Measure.Weights where

import qualified Data.List.NonEmpty as NonEmpty (filter, toList)

import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (ViewGeometry))
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))

effectiveWeight :: ViewState -> ViewGeometry -> Maybe Float
effectiveWeight (ViewState minimized) (ViewGeometry _ _ fixedSize _ weight _) =
  if isJust fixedSize || minimized then Just 0 else weight

amendWeights :: NonEmpty (Maybe Float) -> NonEmpty Float
amendWeights weights =
  fmap (fromMaybe emptyWeight) weights
  where
    total =
      sum (catMaybes (NonEmpty.toList weights))
    normTotal =
      if total == 0 then 1 else total
    empties =
      length (NonEmpty.filter isNothing weights)
    emptyWeight =
      fromMaybe normTotal (normTotal / realToFrac empties)

normalizeWeights :: NonEmpty Float -> NonEmpty Float
normalizeWeights weights =
  fmap norm weights
  where
    norm a =
      fromMaybe a (a / sum weights)

amendAndNormalizeWeights :: NonEmpty (Maybe Float) -> NonEmpty Float
amendAndNormalizeWeights = normalizeWeights . amendWeights

viewWeights :: NonEmpty (ViewState, ViewGeometry) -> NonEmpty Float
viewWeights = amendAndNormalizeWeights . fmap (uncurry effectiveWeight)
