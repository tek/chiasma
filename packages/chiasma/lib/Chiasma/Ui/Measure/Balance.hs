module Chiasma.Ui.Measure.Balance where

import Data.List (zipWith3)
import qualified Data.List.NonEmpty as NonEmpty

import Chiasma.Ui.Measure.Weights (
  amendAndNormalizeWeights,
  normalizeWeights,
  )

zipWith3NE :: (a -> b -> c -> d)  ->  NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
zipWith3NE z ~(a :| as) ~(b :| bs) ~(c :| cs) =
  z a b c :| zipWith3 z as bs cs

data Balance =
  Balance {
    balanceMin :: NonEmpty Float,
    balanceMax :: NonEmpty (Maybe Float),
    balanceWeights :: NonEmpty Float,
    balanceMinimized :: NonEmpty Bool,
    balanceTotal :: Float
  }

reverseWeights :: NonEmpty Float -> NonEmpty Float
reverseWeights weights =
  rev <$> r
  where
    rev a =
      fromMaybe a (a / norm)
    r =
      fmap (1 -) weights
    norm =
      sum r

cutSizes :: Balance -> NonEmpty Float
cutSizes (Balance minSizes _ weights _ total) =
  addNegatives <$> truncatedWeighted
  where
    surplus =
      sum minSizes - total
    surplusDistWeighted =
      fmap (surplus *) (reverseWeights weights)
    truncatedWeighted =
      fmap (uncurry (-)) (NonEmpty.zip minSizes surplusDistWeighted)
    negOrZero a =
      if a < 0 then a else 0
    neg =
      fmap negOrZero truncatedWeighted
    negTotal =
      sum neg
    negCount =
      length (NonEmpty.filter (< 0) neg)
    negativesDist =
      fromMaybe 0 (negTotal / realToFrac (length minSizes - negCount))
    addNegatives a =
      if a < 0 then 0 else a + negativesDist

distributeOnUnbounded :: Balance -> NonEmpty Float
distributeOnUnbounded (Balance min' max' weights _ total) =
  NonEmpty.zipWith addWeights initial newWeights
  where
    initial = NonEmpty.zipWith fromMaybe min' max'
    newWeights = normalizeWeights $ NonEmpty.zipWith weightOrZeroIfMax weights max'
    addWeights i w = i + (w * surplus)
    surplus = total - sum initial
    weightOrZeroIfMax w = maybe w (const 0)

weightsWithoutMinimized :: Balance -> NonEmpty Float
weightsWithoutMinimized (Balance _ _ weights minimized _) =
  normalizeWeights zeroIfMinimizedWeights
  where
    zeroIfMinimizedWeights = NonEmpty.zipWith zeroIfMinimized weights minimized
    zeroIfMinimized w m = if m then 0 else w

trimWeights :: NonEmpty Bool -> NonEmpty Float -> NonEmpty Float
trimWeights unsat withoutMinimized =
  amendAndNormalizeWeights onlyUnsat
  where
    onlyUnsat = NonEmpty.zipWith weightIfUnsat unsat withoutMinimized
    weightIfUnsat s w = if s then Just w else Nothing

distRest :: Balance -> Float -> NonEmpty Float -> NonEmpty Float -> NonEmpty Float
distRest balance rest sizes effectiveMax =
  NonEmpty.zipWith addRestWeights sizes restW
  where
    unsat = NonEmpty.zipWith (>) effectiveMax sizes
    unsatLeft = or unsat
    withoutMinimized = weightsWithoutMinimized balance
    restW =
      if unsatLeft
      then trimWeights unsat withoutMinimized
      else withoutMinimized
    addRestWeights s w = s + w * rest

saturate :: NonEmpty Float -> NonEmpty Float -> NonEmpty Float -> Float -> NonEmpty Float
saturate initial max' initialWeights total =
  loop initial initialWeights
  where
    loop current weights =
      if new == current || rest <= 0 then new else loop new newWeights
      where
        rest = total - sum current
        unsatWeights = zipWith3NE (\s m w -> if s >= m then 0 else w) current max' weights
        newWeights = normalizeWeights unsatWeights
        new = zipWith3NE (\l h w -> min (l + w * rest) h) current max' newWeights

distributeOnAll :: Balance -> NonEmpty Float
distributeOnAll balance@(Balance min' max' weights _ total) =
  if rest <= 0 then sizes else distRest balance rest sizes effectiveMax
  where
    effectiveMax = fromMaybe 1e6 <$> max'
    sizes = saturate min' effectiveMax weights total
    rest = total - sum sizes

hasUnbounded :: Balance -> Bool
hasUnbounded =
  any isNothing . balanceMax

distributeSizes :: Balance -> NonEmpty Float
distributeSizes balance =
  handler balance
  where
    handler =
      if (maxTotal < balanceTotal balance) && hasUnbounded balance
      then distributeOnUnbounded
      else distributeOnAll
    maxTotal = sum (catMaybes $ NonEmpty.toList $ balanceMax balance)

roundSizes :: NonEmpty Float -> NonEmpty Int
roundSizes (h :| t) =
  roundedHead + round surplus :| roundedTail
  where
    (surplus, roundedTail) = mapAccumL folder diff0 t
    (roundedHead, diff0) = diff h
    folder z a =
      (z + z1, a1)
      where
        (a1, z1) = diff a
    diff a = (floor a, a - fromIntegral (floor a :: Int))

-- |Tmux doesn't render panes smaller than two cells.
ensureMinimum2 :: NonEmpty Float -> NonEmpty Float
ensureMinimum2 sizes =
  choose <$> positives
  where
    positive =
      max 0
    positives =
      positive <$> sizes
    overTwoCount =
      length (NonEmpty.filter (>= 2) sizes)
    unders =
      amountUnderTwo <$> positives
    -- If no sizes are larger than 2, nothing will have to be subtracted, all sizes will be clamped to 2
    amountUnderTwoDist =
      fromMaybe 0 (sum unders / realToFrac overTwoCount)
    amountUnderTwo a =
      positive (2 - a)
    choose a =
      max 2 (a - amountUnderTwoDist)

rectifySizes :: NonEmpty Float -> NonEmpty Int
rectifySizes =
  roundSizes . ensureMinimum2

-- FIXME need to round manually, keeping track of the surplus, in order to achieve determinism
balanceSizes :: NonEmpty Float -> NonEmpty (Maybe Float) -> NonEmpty Float -> NonEmpty Bool -> Float -> NonEmpty Int
balanceSizes minSizes maxSizes weights minimized total =
  rectifySizes (fit balance)
  where
    fit = if sum minSizes > total then cutSizes else distributeSizes
    balance = Balance minSizes maxSizes weights minimized total
