module Chiasma.Data.SessionParams where

import Path (Abs, Dir, Path)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), arg, envVars, flag1, identOption, optionWith)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.SessionGroupId (SessionGroupId (unSessionGroupId))
import Chiasma.Path (pathText)

data SessionParams =
  SessionParams {
    attach :: Bool,
    defaultSize :: Bool,
    detach :: Bool,
    exitClient :: Bool,
    printInfo :: Bool,
    cwd :: Maybe (Path Abs Dir),
    environment :: Map Text Text,
    name :: Maybe Ident,
    windowName :: Maybe Ident,
    target :: Maybe SessionGroupId,
    width :: Maybe Int,
    height :: Maybe Int,
    command :: Maybe Text
  }
  deriving stock (Eq, Show)

instance Default SessionParams where
  def =
    SessionParams {
      attach = False,
      defaultSize = False,
      detach = False,
      exitClient = False,
      printInfo = True,
      cwd = Nothing,
      environment = mempty,
      name = Nothing,
      windowName = Nothing,
      target = Nothing,
      width = Nothing,
      height = Nothing,
      command = Nothing
    }

instance CmdArgs SessionParams where
  cmdArgs SessionParams {..} =
    flag1 "-A" attach
    <>
    flag1 "-d" defaultSize
    <>
    flag1 "-D" detach
    <>
    envVars environment
    <>
    flag1 "-P" printInfo
    <>
    flag1 "-X" exitClient
    <>
    optionWith "-c" pathText cwd
    <>
    identOption "-n" windowName
    <>
    identOption "-s" name
    <>
    optionWith "-t" unSessionGroupId target
    <>
    optionWith "-x" show width
    <>
    optionWith "-y" show height
    <>
    arg command
