module Chiasma.Effect.Codec where

data Codec (command :: Type -> Type) (encode :: Type -> Type) (decode :: Type -> Type) :: Effect where
  Encode :: command a -> Codec command encode decode m (encode a)
  Decode :: command a -> encode a -> decode a -> Codec command encode decode m a

makeSem ''Codec
