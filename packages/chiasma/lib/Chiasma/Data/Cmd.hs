module Chiasma.Data.Cmd(
  CmdName(..),
  CmdArgs(..),
  Cmd(..),
  Cmds(..),
  cmd,
) where

newtype CmdName =
  CmdName Text
  deriving stock (Eq, Show)

newtype CmdArgs =
  CmdArgs [Text]
  deriving stock (Eq, Show)

data Cmd =
  Cmd CmdName CmdArgs
  deriving stock (Eq, Show)

newtype Cmds =
  Cmds [Cmd]
  deriving stock (Eq, Show)

cmd :: Text -> [Text] -> Cmd
cmd name args = Cmd (CmdName name) (CmdArgs args)
