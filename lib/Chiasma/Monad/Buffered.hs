module Chiasma.Monad.Buffered(
  runTmux,
) where

import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Default.Class (Default(def))
import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Data.TmuxThunk (Cmd(..), Cmds(..), TmuxThunk(..), TmuxError)
import Chiasma.Monad.Tmux (TmuxProg)

newtype TmuxState = TmuxState [Cmd]

instance Default TmuxState where
  def = TmuxState def

interpret :: (MonadIO m, TmuxApi api) => TmuxState -> api -> TmuxProg a b -> ExceptT TmuxError m b
interpret (TmuxState cmds) api (Pure a) = a <$ runCommands api (const $ Right ()) (Cmds cmds)
interpret (TmuxState cmds) api (Free (Read cmd decode next)) = do
  a <- runCommands api decode $ Cmds (cmd : cmds)
  interpret def api (next a)
interpret _ _ _ = undefined

runTmux :: (MonadIO m, TmuxApi api) => api -> TmuxProg a b -> m (Either TmuxError b)
runTmux api prog = runExceptT $ interpret def api prog
