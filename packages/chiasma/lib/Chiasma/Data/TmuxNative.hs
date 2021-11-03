module Chiasma.Data.TmuxNative where

import Path (Abs, File, Path)

data TmuxNative =
  TmuxNative {
    executable :: Path Abs File,
    tmuxServerSocket :: Maybe (Path Abs File)
  }
  deriving stock (Eq, Show)
