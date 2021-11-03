module Chiasma.Codec where

import qualified Data.Text as Text (drop, dropEnd, splitOn, take, takeEnd, unwords)
import GHC.Generics (Rep, to)

import Chiasma.Codec.Decode (TmuxDataDecode (..))
import Chiasma.Codec.Query (TmuxDataQuery (..))
import qualified Chiasma.Data.DecodeError as DecodeFailure (DecodeFailure (..))
import Chiasma.Data.DecodeError (DecodeError (DecodeError))
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.TmuxQuery (TmuxQuery (TmuxQuery))

genDecode ::
  Generic a =>
  TmuxDataDecode (Rep a) =>
  Text ->
  Either DecodeError a
genDecode fields = do
  first (DecodeError [fields]) (check =<< decode' (Text.splitOn " " (trim fields)))
  where
    check = \case
      ([], result) -> Right (to result)
      (a, _) -> Left (DecodeFailure.TooManyFields a)
    trim text =
      if Text.take 1 text == " " && Text.takeEnd 1 text == " "
      then Text.drop 1 (Text.dropEnd 1 text)
      else text

class TmuxCodec a where
    decode :: Text -> Either DecodeError a
    default decode ::
      Generic a =>
      TmuxDataDecode (Rep a) =>
      Text ->
      Either DecodeError a
    decode =
      genDecode

    query :: TmuxQuery
    default query :: TmuxDataQuery (Rep a) => TmuxQuery
    query =
      TmuxQuery (Text.unwords (query' @(Rep a)))

instance TmuxCodec SessionId
instance TmuxCodec WindowId
instance TmuxCodec PaneId

multi ::
  TmuxCodec a =>
  [Text] ->
  Either DecodeError [a]
multi =
  traverse decode

single ::
  TmuxCodec a =>
  [Text] ->
  Either DecodeError a
single = \case
  [out] -> decode out
  out -> Left (DecodeError out (DecodeFailure.TooManyRecords out))
