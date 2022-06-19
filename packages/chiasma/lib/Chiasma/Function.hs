module Chiasma.Function where

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x =
  f x
applyWhen False _ x =
  x
