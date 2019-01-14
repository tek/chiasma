module Chiasma.Ui.Measure.Balance(
  balanceSizes,
  Balance(..),
) where

import GHC.Float (int2Float)
import Data.Maybe (catMaybes, isNothing, fromMaybe)
import Chiasma.Ui.Measure.Weights (normalizeWeights, amendAndNormalizeWeights)

data Balance =
  Balance {
    balanceMin :: [Float],
    balanceMax :: [Maybe Float],
    balanceWeights :: [Float],
    balanceMinimized :: [Bool],
    balanceTotal :: Float
  }

reverseWeights :: [Float] -> [Float]
reverseWeights weights =
  if norm > 0 then fmap (/ norm) r else r
  where
    r = fmap (1 -) weights
    norm = sum r

cutSizes :: Balance -> [Float]
cutSizes (Balance min' _ weights _ total) =
  fmap addDist cut
  where
    surplus = sum min' - total
    dist = fmap (surplus *) (reverseWeights weights)
    cut = fmap (uncurry (-)) (zip min' dist)
    negOrZero a = if a < 0 then a else 0
    neg = fmap negOrZero cut
    negTotal = sum neg
    negCount = length (filter (< 0) neg)
    dist2 = negTotal / int2Float (length min' - negCount)
    addDist a = if a < 0 then 0 else a + dist2

distributeOnUnbounded :: Balance -> [Float]
distributeOnUnbounded (Balance min' max' weights _ total) =
  zipWith addWeights initial newWeights
  where
    initial = zipWith fromMaybe min' max'
    newWeights = normalizeWeights $ zipWith weightOrZeroIfMax weights max'
    addWeights i w = i + (w * surplus)
    surplus = total - sum initial
    weightOrZeroIfMax w = maybe w (const 0)

weightsWithoutMinimized :: Balance -> [Float]
weightsWithoutMinimized (Balance _ _ weights minimized _) =
  normalizeWeights zeroIfMinimizedWeights
  where
    zeroIfMinimizedWeights = zipWith zeroIfMinimized weights minimized
    zeroIfMinimized w m = if m then 0 else w

trimWeights :: [Bool] -> [Float] -> [Float]
trimWeights unsat withoutMinimized =
  amendAndNormalizeWeights onlyUnsat
  where
    onlyUnsat = zipWith weightIfUnsat unsat withoutMinimized
    weightIfUnsat s w = if s then Just w else Nothing

distRest :: Balance -> Float -> [Float] -> [Float] -> [Float]
distRest balance rest sizes effectiveMax =
  zipWith addRestWeights sizes restW
  where
    unsat = zipWith (>) effectiveMax sizes
    unsatLeft = or unsat
    withoutMinimized = weightsWithoutMinimized balance
    restW =
      if unsatLeft
      then trimWeights unsat withoutMinimized
      else withoutMinimized
    addRestWeights s w = s + w * rest

saturate :: [Float] -> [Float] -> [Float] -> Float -> [Float]
saturate initial max' initialWeights total =
  loop initial initialWeights
  where
    loop current weights =
      if new == current || rest <= 0 then new else loop new newWeights
      where
        rest = total - sum current
        unsatWeights = zipWith3 (\s m w -> if s >= m then 0 else w) current max' weights
        newWeights = normalizeWeights unsatWeights
        new = zipWith3 (\l h w -> min (l + w * rest) h) current max' newWeights

distributeOnAll :: Balance -> [Float]
distributeOnAll balance@(Balance min' max' weights _ total) =
  if rest <= 0 then sizes else distRest balance rest sizes effectiveMax
  where
    effectiveMax = fromMaybe 10e6 <$> max'
    sizes = saturate min' effectiveMax weights total
    rest = total - sum sizes

hasUnbounded :: Balance -> Bool
hasUnbounded =
  any isNothing . balanceMax

distributeSizes :: Balance -> [Float]
distributeSizes balance =
  handler balance
  where
    handler =
      if (maxTotal < balanceTotal balance) && hasUnbounded balance
      then distributeOnUnbounded
      else distributeOnAll
    maxTotal = sum (catMaybes $ balanceMax balance)

rectifySizes :: [Float] -> [Float]
rectifySizes sizes =
  fmap choose positives
  where
    positive = max 0
    positives = fmap positive sizes
    positivesCount = length $ filter (>= 2) sizes
    unders = fmap amountUnderTwo positives
    sub = sum unders / int2Float positivesCount
    amountUnderTwo a = positive (2 - a)
    choose a = max 2 (a - sub)

balanceSizes :: [Float] -> [Maybe Float] -> [Float] -> [Bool] -> Float -> [Float]
balanceSizes minSizes maxSizes weights minimized total =
  rectifySizes (fit balance)
  where
    fit = if sum minSizes > total then cutSizes else distributeSizes
    balance = Balance minSizes maxSizes weights minimized total
