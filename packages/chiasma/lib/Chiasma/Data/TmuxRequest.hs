module Chiasma.Data.TmuxRequest where

import qualified Data.Text as Text
import Chiasma.Data.TmuxQuery (TmuxQuery (TmuxQuery))

data TmuxRequest =
  TmuxRequest {
    cmd :: Text,
    args :: [Text],
    query :: Maybe TmuxQuery
  }
  deriving stock (Eq, Show)

queryArgs :: TmuxQuery -> [Text]
queryArgs (TmuxQuery q) =
  ["-F", "'", q, "'"]

cmdline :: TmuxRequest -> [Text]
cmdline TmuxRequest { ..} =
  cmd : args ++ foldMap queryArgs query ++ ["\n"]

encode :: TmuxRequest -> ByteString
encode req =
  encodeUtf8 (Text.unwords (cmdline req))
