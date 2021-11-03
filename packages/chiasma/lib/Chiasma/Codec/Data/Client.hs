module Chiasma.Codec.Data.Client where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (ClientId, SessionId)

data Client =
  Client {
    clientName :: ClientId,
    clientControlMode :: Bool,
    sessionId :: SessionId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)
