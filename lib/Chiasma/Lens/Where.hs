{-# LANGUAGE RankNTypes #-}

module Chiasma.Lens.Where(
  where1,
) where

import Control.Lens (Over, filtered, taking)

where1 :: (Applicative f, Traversable t) => (a -> Bool) -> Over (->) f (t a) (t a) a a
where1 predicate = taking 1 $ traverse . filtered predicate
