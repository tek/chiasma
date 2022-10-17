module Chiasma.Interpreter.Pure where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Interpreter.Codec (interpretCodecPure)
import Chiasma.Interpreter.TmuxClient (interpretTmuxClientNull)

interpretTmuxPure ::
  (∀ a . command a -> Sem r (Either Text a)) ->
  InterpretersFor [Scoped_ (TmuxClient () ()) !! TmuxError, Codec command () () !! Text] r
interpretTmuxPure run =
  interpretCodecPure run . interpretTmuxClientNull
