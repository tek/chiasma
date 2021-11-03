module Chiasma.Path where

import Path (Path, toFilePath)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath
{-# inline pathText #-}
