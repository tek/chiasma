module Chiasma.Lens.Where where

import Control.Lens (Over, taking)

where1 :: (Applicative f, Traversable t) => (a -> Bool) -> Over (->) f (t a) (t a) a a
where1 predicate = taking 1 $ traverse . filtered predicate
