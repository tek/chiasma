module Chiasma.Effect.Codec where

import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxRequest (TmuxRequest)

data Codec (command :: Type -> Type) (encode :: Type -> Type) (decode :: Type -> Type) :: Effect where
  Encode :: command a -> Codec command encode decode m (encode a)
  Decode :: command a -> encode a -> decode a -> Codec command encode decode m a

makeSem ''Codec

type NativeCodec command =
  Codec command (Const TmuxRequest) (Const [Text])

type NativeCommandCodec =
  NativeCodec TmuxCommand
