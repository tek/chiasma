module Chiasma.Effect.TmuxApi where

import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxNotification (TmuxNotification)

data TmuxApi (command :: Type -> Type) :: Effect where
  Send :: command a -> TmuxApi command m a
  Schedule :: command a -> TmuxApi command m ()
  -- | Block until the next tmux control mode notification (e.g. @%window-add@, @%layout-changed@).
  ReceiveNotification :: TmuxApi command m TmuxNotification

makeSem ''TmuxApi

type Tmux =
  TmuxApi TmuxCommand
