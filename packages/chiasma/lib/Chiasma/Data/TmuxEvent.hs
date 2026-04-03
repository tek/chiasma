module Chiasma.Data.TmuxEvent where

import Chiasma.Data.TmuxNotification (TmuxNotification)
import Chiasma.Data.TmuxOutputBlock (TmuxOutputBlock)

data TmuxEvent =
  Response TmuxOutputBlock
  |
  Notification TmuxNotification
  deriving stock (Eq, Show)
