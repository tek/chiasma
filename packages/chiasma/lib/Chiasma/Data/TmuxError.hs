module Chiasma.Data.TmuxError where

import Polysemy.Process.Data.ProcessError (ProcessError)

import Chiasma.Data.CodecError (CodecError (CodecError))
import Chiasma.Data.DecodeError (DecodeError)
import Chiasma.Data.TmuxRequest (TmuxRequest)

data TmuxError =
  ProcessFailed ProcessError
  |
  RequestFailed TmuxRequest [Text]
  |
  DecodeFailed TmuxRequest DecodeError
  |
  NoClients
  |
  NoExe
  deriving stock (Eq, Show)

codec :: CodecError -> TmuxError
codec (CodecError req err) =
  DecodeFailed req err
