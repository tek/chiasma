module Chiasma.Data.CodecError where

import Chiasma.Data.DecodeError (DecodeError)
import Chiasma.Data.TmuxRequest (TmuxRequest)

data CodecError =
  CodecError TmuxRequest DecodeError
  deriving stock (Eq, Show)
