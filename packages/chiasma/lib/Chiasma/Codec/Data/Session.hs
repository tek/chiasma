module Chiasma.Codec.Data.Session where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (SessionId)

-- must be data for now because of how derivation of `TmuxCodec` is implemented
data Session =
  Session {
    sessionId :: SessionId,
    sessionName :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)
