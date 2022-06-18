module Chiasma.Effect.TmuxClient where

import Prelude hiding (send)

import Chiasma.Data.TmuxRequest (TmuxRequest)

data TmuxClient (encode :: Type -> Type) (decode :: Type -> Type) :: Effect where
  Send :: encode a -> TmuxClient encode decode m (decode a)
  Schedule :: encode a -> TmuxClient encode decode m ()

makeSem ''TmuxClient

type ScopedTmux resource encode decode =
  Scoped resource (TmuxClient encode decode)

type NativeTmux =
  ScopedTmux () (Const TmuxRequest) (Const [Text])

flush ::
  Member (TmuxClient e d) r =>
  InterpreterFor (TmuxClient e d) r
flush =
  interpret \case
    Send cmd ->
      send cmd
    Schedule cmd ->
      void (send cmd)
