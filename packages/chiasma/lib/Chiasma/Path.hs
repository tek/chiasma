module Chiasma.Path where

import qualified Data.Text as Text
import Path (Path, toFilePath)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath
{-# inline pathText #-}

pathText' :: Path b t -> Text
pathText' p =
  case pathText p of
    "/" -> "/"
    t -> Text.dropWhileEnd (== '/') t
{-# inline pathText' #-}
