module Chiasma.Effect.TmuxApi where

import Chiasma.Data.TmuxCommand (TmuxCommand)

data TmuxApi (command :: Type -> Type) :: Effect where
  Send :: command a -> TmuxApi command m a
  Schedule :: command a -> TmuxApi command m ()

makeSem ''TmuxApi

type Tmux =
  TmuxApi TmuxCommand
