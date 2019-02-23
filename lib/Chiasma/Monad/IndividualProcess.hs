module Chiasma.Monad.IndividualProcess(
  TmuxProg,
  runTmux,
) where

import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Data.TmuxThunk (Cmd(..), Cmds(..), TmuxThunk(..), TmuxError)
import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Default.Class (Default(def))

type TmuxProg = Free TmuxThunk

newtype TmuxState = TmuxState [Cmd]

instance Default TmuxState where
  def = TmuxState def

interpret :: (MonadIO m, TmuxApi api) => TmuxState -> api -> TmuxProg a -> ExceptT TmuxError m a
interpret (TmuxState cmds) api (Pure a) = a <$ runCommands api (const $ Right ()) (Cmds cmds)
interpret (TmuxState cmds) api (Free (Read cmd decode next)) = do
  a <- runCommands api decode $ Cmds (cmd : cmds)
  interpret def api (next a)
interpret (TmuxState cmds) api (Free (Write cmd next)) =
  interpret (TmuxState (cmd : cmds)) api (next ())
interpret _ _ (Free (Failed err)) =
  throwE err

runTmux :: (MonadIO m, TmuxApi api) => api -> TmuxProg a -> m (Either TmuxError a)
runTmux api prog = runExceptT $ interpret def api prog
