module Chiasma.Native.Api(
  TmuxApi(..),
  TmuxNative(..),
) where

import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Text.ParserCombinators.Parsec ()
import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Native.Process (nativeTmuxProcess)

newtype TmuxNative = TmuxNative FilePath

instance TmuxApi TmuxNative where
  runCommands (TmuxNative socket) decode cmds = ExceptT $ nativeTmuxProcess socket decode cmds
