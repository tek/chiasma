module Chiasma.Codec where

import qualified Data.Text as Text
import GHC.Generics (Rep, to)
import Prelude hiding (to)

import Chiasma.Codec.Decode (TmuxDataDecode (..))
import Chiasma.Codec.Query (TmuxDataQuery (..))
import qualified Chiasma.Data.DecodeError as DecodeFailure (DecodeFailure (..))
import Chiasma.Data.DecodeError (DecodeError (DecodeError), DecodeFailure)
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.TmuxQuery (TmuxQuery (TmuxQuery))

-- |Remove one leading and trailing space from tmux output if both are present.
tryTrim :: Text -> Maybe Text
tryTrim text = do
  (prefix, lastChar) <- Text.unsnoc text
  guard (lastChar == ' ')
  (firstChar, payload) <- Text.uncons prefix
  guard (firstChar == ' ')
  pure payload

trim :: Text -> Text
trim text =
  fromMaybe text (tryTrim text)

checkRemainder :: ([Text], b) -> Either DecodeFailure b
checkRemainder = \case
  ([], result) -> Right result
  (a, _) -> Left (DecodeFailure.TooManyFields a)

genDecode ::
  Generic a =>
  TmuxDataDecode (Rep a) =>
  Text ->
  Either DecodeError a
genDecode fields = do
  bimap (DecodeError [fields]) to do
    checkRemainder =<< dataDecode (Text.splitOn " " (trim fields))

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
    TmuxQuery (Text.unwords (dataQuery @(Rep a)))

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
  [] -> Left (DecodeError [] DecodeFailure.TargetMissing)
  out -> Left (DecodeError out (DecodeFailure.TooManyRecords out))
