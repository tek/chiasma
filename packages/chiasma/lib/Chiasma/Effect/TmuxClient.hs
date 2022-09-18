module Chiasma.Effect.TmuxClient where

import Prelude hiding (send)

import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.TmuxResponse (TmuxResponse)

data TmuxClient (i :: Type) (o :: Type) :: Effect where
  Send :: i -> TmuxClient i o m o
  Schedule :: i -> TmuxClient i o m ()

makeSem ''TmuxClient

type ScopedTmux resource i o =
  Scoped_ resource (TmuxClient i o)

type NativeTmux =
  ScopedTmux () TmuxRequest TmuxResponse

flush ::
  Member (TmuxClient e d) r =>
  InterpreterFor (TmuxClient e d) r
flush =
  interpret \case
    Send cmd ->
      send cmd
    Schedule cmd ->
      void (send cmd)
