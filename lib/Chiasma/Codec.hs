{-# LANGUAGE AllowAmbiguousTypes #-}

module Chiasma.Codec where

import Data.Text (Text)
import qualified Data.Text as T (drop, dropEnd, splitOn, take, takeEnd)
import GHC.Generics (Generic, Rep, to)

import Chiasma.Codec.Decode (TmuxDataDecode(..), TmuxDecodeError(TooManyFields))
import Chiasma.Codec.Query (TmuxDataQuery(..))
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)

newtype TmuxQuery =
  TmuxQuery { unQ :: String }
  deriving (Eq, Show)

genDecode :: (Generic a, TmuxDataDecode (Rep a)) => Text -> Either TmuxDecodeError a
genDecode fields = do
  (rest, result) <- decode' (T.splitOn " " . trim $ fields)
  case rest of
    [] -> return $ to result
    a -> Left $ TooManyFields a
  where
    trim text =
      if T.take 1 text == " " && T.takeEnd 1 text == " "
      then T.drop 1 . T.dropEnd 1 $ text
      else text

class TmuxCodec a where
    decode :: Text -> Either TmuxDecodeError a
    default decode :: (Generic a, TmuxDataDecode (Rep a)) => Text -> Either TmuxDecodeError a
    decode = genDecode

    query :: TmuxQuery
    default query :: (Generic a, TmuxDataQuery (Rep a)) => TmuxQuery
    query = TmuxQuery $ unwords $ query' @(Rep a)

instance TmuxCodec SessionId
instance TmuxCodec WindowId
instance TmuxCodec PaneId
