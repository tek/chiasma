module Chiasma.Api.Class(
  TmuxApi(..),
  DecodeTmuxResponse(..),
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT)
import Chiasma.Data.TmuxThunk (Cmds, TmuxError)

class TmuxApi a where
  runCommands :: (MonadIO m) => a -> (String -> Either TmuxError b) -> Cmds -> ExceptT TmuxError m [b]

class DecodeTmuxResponse a where
  decode :: String -> Either TmuxError a
