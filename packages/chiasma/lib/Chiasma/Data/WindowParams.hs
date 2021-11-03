module Chiasma.Data.WindowParams where

import Path (Abs, Dir, Path)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), arg, envVars, flag1, identOption, optionWith)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Target (Target)
import Chiasma.Path (pathText)

data WindowParams =
  WindowParams {
    after :: Bool,
    detach :: Bool,
    killExisting :: Bool,
    printInfo :: Bool,
    cwd :: Maybe (Path Abs Dir),
    environment :: Map Text Text,
    name :: Maybe Ident,
    target :: Target,
    command :: Maybe Text
  }
  deriving stock (Eq, Show)

instance Default WindowParams where
  def =
    WindowParams {
      after = False,
      detach = False,
      killExisting = False,
      printInfo = True,
      cwd = Nothing,
      environment = mempty,
      name = Nothing,
      target = def,
      command = Nothing
    }

instance CmdArgs WindowParams where
  cmdArgs WindowParams {..} =
    flag1 "-a" after
    <>
    flag1 "-d" detach
    <>
    flag1 "-k" killExisting
    <>
    flag1 "-P" printInfo
    <>
    optionWith "-c" pathText cwd
    <>
    envVars environment
    <>
    identOption "-n" name
    <>
    cmdArgs target
    <>
    arg command
