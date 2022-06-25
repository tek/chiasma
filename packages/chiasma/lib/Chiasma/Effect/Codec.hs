module Chiasma.Effect.Codec where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxRequest (TmuxRequest)

data Codec (command :: Type -> Type) (encode :: Type -> Type) (decode :: Type -> Type) :: Effect where
  Encode :: command a -> Codec command encode decode m (encode a)
  Decode :: command a -> encode a -> decode a -> Codec command encode decode m a

makeSem ''Codec

type NativeCodec command =
  Codec command (Const TmuxRequest) (Const [Text])

type NativeCodecE command =
  NativeCodec command !! CodecError

type NativeCommandCodec =
  NativeCodec TmuxCommand

type NativeCommandCodecE =
  NativeCodecE TmuxCommand

type family NativeCodecs (cs :: [Type -> Type]) :: [Effect] where
  NativeCodecs '[] = '[]
  NativeCodecs (c : cs) = NativeCodec c : NativeCodecs cs

type family NativeCodecsE (cs :: [Type -> Type]) :: [Effect] where
  NativeCodecsE '[] = '[]
  NativeCodecsE (c : cs) = NativeCodecE c : NativeCodecsE cs
