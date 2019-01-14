module Chiasma.Data.Maybe(
  maybeExcept,
  findMaybe,
) where

import Control.Monad.Error.Class (MonadError(throwError))
import Data.Maybe (mapMaybe)

maybeExcept :: MonadError e m => e -> Maybe a -> m a
maybeExcept e Nothing = throwError e
maybeExcept _ (Just a) = return a

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f as =
  case mapMaybe f as of
    (a : _) -> Just a
    _ -> Nothing
