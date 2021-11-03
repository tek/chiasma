module Chiasma.Data.Event where

import Chiasma.Data.TmuxId (WindowId)

data Event =
  WindowClosed WindowId
  deriving stock (Eq, Show)
