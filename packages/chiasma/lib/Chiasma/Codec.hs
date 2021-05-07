module Chiasma.Codec where

import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import qualified Data.Text as Text (drop, dropEnd, splitOn, take, takeEnd, unwords)
import GHC.Generics (Rep, to)

import Chiasma.Codec.Decode (TmuxDataDecode(..), TmuxDecodeError(TooManyFields))
import Chiasma.Codec.Query (TmuxDataQuery(..))

newtype TmuxQuery =
  TmuxQuery { unQ :: Text }
  deriving (Eq, Show)

genDecode :: (Generic a, TmuxDataDecode (Rep a)) => Text -> Either TmuxDecodeError a
genDecode fields = do
  (rest, result) <- decode' (Text.splitOn " " . trim $ fields)
  case rest of
    [] -> return $ to result
    a -> Left $ TooManyFields a
  where
    trim text =
      if Text.take 1 text == " " && Text.takeEnd 1 text == " "
      then Text.drop 1 . Text.dropEnd 1 $ text
      else text

class TmuxCodec a where
    decode :: Text -> Either TmuxDecodeError a
    default decode :: (Generic a, TmuxDataDecode (Rep a)) => Text -> Either TmuxDecodeError a
    decode = genDecode

    query :: TmuxQuery
    default query :: TmuxDataQuery (Rep a) => TmuxQuery
    query = TmuxQuery $ Text.unwords $ query' @(Rep a)

instance TmuxCodec SessionId
instance TmuxCodec WindowId
instance TmuxCodec PaneId
