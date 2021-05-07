module Chiasma.Data.Cmd(
  CmdName(..),
  CmdArgs(..),
  Cmd(..),
  Cmds(..),
  cmd,
) where

newtype CmdName =
  CmdName Text
  deriving (Eq, Show)

newtype CmdArgs =
  CmdArgs [Text]
  deriving (Eq, Show)

data Cmd =
  Cmd CmdName CmdArgs
  deriving (Eq, Show)

newtype Cmds =
  Cmds [Cmd]
  deriving (Eq, Show)

cmd :: Text -> [Text] -> Cmd
cmd name args = Cmd (CmdName name) (CmdArgs args)
