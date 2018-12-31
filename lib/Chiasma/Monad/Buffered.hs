module Chiasma.Monad.Buffered(
  runTmux,
) where

import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class (Default(def))
import Chiasma.Api (TmuxApi(..))
import Chiasma.Data.TmuxThunk (Cmd(..), Args(..), TmuxThunk(..))
import Chiasma.Monad.Tmux (TmuxProg)

newtype TmuxState = TmuxState [(Cmd, Args)]

instance Default TmuxState where
  def = TmuxState def

interpret :: (MonadIO m, TmuxApi api) => TmuxState -> api -> TmuxProg a b -> m b
interpret (TmuxState cmds) api (Pure a) = a <$ runCommands api cmds
interpret (TmuxState cmds) api (Free (Read cmd args next)) = do
  a <- runCommands api $ (cmd, args) : cmds
  interpret def api (next a)
interpret _ _ _ = undefined

runTmux :: (MonadIO m, TmuxApi api) => api -> TmuxProg a b -> m b
runTmux = interpret def
