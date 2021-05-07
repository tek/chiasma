module Chiasma.Api.Class where

import Conduit (ConduitT, Flush)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmd, Cmds)
import Chiasma.Native.StreamParse (TmuxOutputBlock)

class TmuxApi m a where
  runCommands ::
    a ->
    (Text -> Either TmuxDecodeError b) ->
    Cmds ->
    m [b]

  withTmux ::
    a ->
    (ConduitT (Flush Cmd) Void m () -> ConduitT () TmuxOutputBlock m () -> m b) ->
    m b
