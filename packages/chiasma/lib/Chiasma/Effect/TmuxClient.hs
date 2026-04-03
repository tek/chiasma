module Chiasma.Effect.TmuxClient where

import Prelude hiding (send)

import Chiasma.Data.TmuxNotification (TmuxNotification)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.TmuxResponse (TmuxResponse)

data TmuxClient (i :: Type) (o :: Type) :: Effect where
  Send :: i -> TmuxClient i o m o
  Schedule :: i -> TmuxClient i o m ()
  -- | Block until the next tmux control mode notification (e.g. @%window-add@, @%layout-changed@).
  ReceiveNotification :: TmuxClient i o m TmuxNotification

makeSem ''TmuxClient

type ScopedTmux i o =
  Scoped_ (TmuxClient i o)

type NativeTmux =
  ScopedTmux TmuxRequest TmuxResponse
