module Chiasma.Data.Maybe where

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

orElse :: Maybe a -> Maybe a -> Maybe a
orElse _ (Just a) = Just a
orElse fallback Nothing = fallback
