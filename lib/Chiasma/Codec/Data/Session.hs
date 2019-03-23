{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data.Session where

import GHC.Generics (Generic)

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (SessionId)

-- must be data for now because of how derivation of `TmuxCodec` is implemented
data Session =
  Session {
    sessionId :: SessionId
  }
  deriving (Eq, Show, Generic, TmuxCodec)
