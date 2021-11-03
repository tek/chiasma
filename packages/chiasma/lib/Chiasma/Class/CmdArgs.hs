module Chiasma.Class.CmdArgs where

import qualified Data.Map.Strict as Map
import Exon (exon)

import Chiasma.Data.Ident (Ident, identText)

class CmdArgs a where
  cmdArgs :: a -> [Text]

flag :: [Text] -> Bool -> [Text]
flag =
  bool []

flag1 :: Text -> Bool -> [Text]
flag1 =
  flag . pure

optionWith :: Text -> (a -> Text) -> Maybe a -> [Text]
optionWith flg f =
  foldMap \ a -> [flg, f a]

option :: Text -> Maybe Text -> [Text]
option flg =
  optionWith flg id

identOption :: Text -> Maybe Ident -> [Text]
identOption flg =
  optionWith flg identText

arg :: Maybe Text -> [Text]
arg =
  maybeToList

envVars :: Map Text Text -> [Text]
envVars =
  concatMap \case
    (k, v) -> ["-e", [exon|#{k}=#{v}|]]
  .
  Map.toList

optionArgs ::
  CmdArgs a =>
  Maybe a ->
  [Text]
optionArgs =
  foldMap cmdArgs

optionArgsWith ::
  CmdArgs a =>
  Text ->
  Maybe a ->
  [Text]
optionArgsWith flg =
  foldMap \ a -> flg : cmdArgs a
