module Chiasma.Ui.Measure.Balance where

import Data.List (zipWith3)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty (
  filter,
  toList,
  zip,
  zipWith,
  )
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Traversable (mapAccumL)
import GHC.Float (float2Int, int2Float)
import GHC.Float.RealFracMethods (floorFloatInt)

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
  if norm > 0 then fmap (/ norm) r else r
  where
    r = fmap (1 -) weights
    norm = sum r

cutSizes :: Balance -> NonEmpty Float
cutSizes (Balance min' _ weights _ total) =
  fmap addDist cut
  where
    surplus = sum min' - total
    dist = fmap (surplus *) (reverseWeights weights)
    cut = fmap (uncurry (-)) (NonEmpty.zip min' dist)
    negOrZero a = if a < 0 then a else 0
    neg = fmap negOrZero cut
    negTotal = sum neg
    negCount = length (NonEmpty.filter (< 0) neg)
    dist2 = negTotal / int2Float (length min' - negCount)
    addDist a = if a < 0 then 0 else a + dist2

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
roundSizes (head' :| tail') =
  roundedHead + (float2Int surplus) :| roundedTail
  where
    (surplus, roundedTail) = mapAccumL folder diff0 tail'
    (roundedHead, diff0) = diff head'
    folder z a =
      (z + z1, a1)
      where
        (a1, z1) = diff a
    diff a = (floorFloatInt a, a - int2Float (floor a))

ensureMinimum2 :: NonEmpty Float -> NonEmpty Float
ensureMinimum2 sizes =
  choose <$> positives
  where
    positive = max 0
    positives = positive <$> sizes
    positivesCount = length $ NonEmpty.filter (>= 2) sizes
    unders = amountUnderTwo <$> positives
    sub = sum unders / int2Float positivesCount
    amountUnderTwo a = positive (2 - a)
    choose a = max 2 (a - sub)

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
