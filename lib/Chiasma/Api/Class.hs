module Chiasma.Api.Class where

import Conduit (ConduitT, Flush, Void)
import Data.Text (Text)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmd, Cmds)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Native.StreamParse (TmuxOutputBlock)

class TmuxApi m a where
  runCommands ::
    a ->
    ([Text] -> Either TmuxDecodeError b) ->
    Cmds ->
    m [b]

  withTmux ::
    a ->
    (ConduitT (Flush Cmd) Void m () -> ConduitT () TmuxOutputBlock m () -> m b) ->
    m b

class DecodeTmuxResponse a where
  decode :: String -> Either TmuxError a
