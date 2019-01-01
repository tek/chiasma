module Chiasma.Api.Class(
  TmuxApi(..),
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT)
import Chiasma.Data.TmuxThunk (Cmds, TmuxError)

class TmuxApi a where
  runCommands :: (MonadIO m) => a -> Cmds -> ExceptT TmuxError m [[String]]
