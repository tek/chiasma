module Chiasma.Effect.Codec where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.TmuxResponse (TmuxResponse)

data Codec (command :: Type -> Type) (i :: Type) (o :: Type) :: Effect where
  WithCodec :: command a -> (i -> m o) -> Codec command i o m a
  Encode :: command a -> Codec command i o m i

makeSem ''Codec

type NativeCodec command =
  Codec command TmuxRequest TmuxResponse

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
