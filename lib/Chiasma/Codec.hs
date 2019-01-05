{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Codec(
  TmuxCodec(..),
) where

import GHC.Generics (Generic, Rep, to)
import Chiasma.Codec.Decode (TmuxDataDecode(..), TmuxDecodeError(TooManyFields))
import Chiasma.Codec.Query (TmuxDataQuery(..), Query(..))

genDecode :: (Generic a, TmuxDataDecode (Rep a)) => [String] -> Either TmuxDecodeError a
genDecode fields = do
  (rest, result) <- decode' fields
  case rest of
    [] -> return $ to result
    a -> Left $ TooManyFields a

class TmuxCodec a where
    decode :: [String] -> Either TmuxDecodeError a
    default decode :: (Generic a, TmuxDataDecode (Rep a)) => [String] -> Either TmuxDecodeError a
    decode = genDecode

    query :: Query a
    default query :: (Generic a, TmuxDataQuery (Rep a)) => Query a
    query = fmap to query'
