{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chiasma.Codec(
  TmuxCodec(..),
  TmuxQuery(..),
) where

import GHC.Generics (Generic, Rep, to)
import Chiasma.Codec.Decode (TmuxDataDecode(..), TmuxDecodeError(TooManyFields))
import Chiasma.Codec.Query (TmuxDataQuery(..))

newtype TmuxQuery =
  TmuxQuery { unQ :: String }
  deriving (Eq, Show)

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

    query :: TmuxQuery
    default query :: (Generic a, TmuxDataQuery (Rep a)) => TmuxQuery
    query = TmuxQuery $ unwords $ query' @(Rep a)
