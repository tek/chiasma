module Chiasma.Data.SendKeysParams where

import qualified Data.Text as Text
import Exon (exon)
import Prelude hiding (repeat)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1, optionWith)
import Chiasma.Data.Target (Target)

escape :: Text -> Text
escape fragment =
  [exon|"#{escapeQuotes}"|]
  where
    escapeQuotes =
      Text.replace [exon|"|] [exon|\"|] fragment

data Key =
  Key Text
  |
  Lit Text
  deriving stock (Eq, Show)

instance IsString Key where
  fromString =
    Lit . toText

instance CmdArgs Key where
  cmdArgs = \case
    Key k -> [k]
    Lit s -> [escape s]

data SendKeysParams =
  SendKeysParams {
    enter :: Bool,
    literal :: Bool,
    hex :: Bool,
    reset :: Bool,
    mouse :: Bool,
    copyMode :: Bool,
    repeat :: Maybe Int,
    keys :: [Key],
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default SendKeysParams where
  def =
    SendKeysParams {
      enter = True,
      literal = False,
      hex = False,
      reset = False,
      mouse = False,
      copyMode = False,
      repeat = Nothing,
      keys = mempty,
      target = def
    }

instance CmdArgs SendKeysParams where
  cmdArgs SendKeysParams {..} =
    flag1 "-l" literal
    <>
    flag1 "-H" hex
    <>
    flag1 "-R" reset
    <>
    flag1 "-M" mouse
    <>
    flag1 "-X" copyMode
    <>
    optionWith "-N" show repeat
    <>
    cmdArgs target
    <>
    (cmdArgs =<< keys)
    <>
    flag1 "enter" (enter && not copyMode)
