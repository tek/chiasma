module Chiasma.Control.IO.Unsafe where

import GHC.IO.Unsafe (unsafePerformIO)

unsafeLog :: Show a => a -> b -> b
unsafeLog a b = unsafePerformIO $ print a >> return b
