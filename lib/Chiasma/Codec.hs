{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Codec(
  TmuxCodec(..),
) where

import GHC.Generics (Generic, Rep, to)
import Chiasma.Codec.Decode (TmuxDataDecode, TmuxDecodeError(TooManyFields), decode')

genericCoder :: (Generic a, TmuxDataDecode (Rep a)) => [String] -> Either TmuxDecodeError a
genericCoder fields = do
  (rest, result) <- decode' fields
  case rest of
    [] -> return $ to result
    a -> Left $ TooManyFields a

class TmuxCodec a where
    decode :: [String] -> Either TmuxDecodeError a
    default decode :: (Generic a, TmuxDataDecode (Rep a)) => [String] -> Either TmuxDecodeError a
    decode = genericCoder
