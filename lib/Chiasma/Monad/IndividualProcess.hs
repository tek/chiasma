module Chiasma.Monad.IndividualProcess(
  TmuxProg,
  runTmux,
) where

import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Default.Class (Default(def))

import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Data.Cmd (Cmd(..), Cmds(..))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk(..))

type TmuxProg = Free TmuxThunk

newtype TmuxState = TmuxState [Cmd]

instance Default TmuxState where
  def = TmuxState def

interpret ::
  (TmuxApi m api, MonadDeepError e TmuxError m) =>
  TmuxState ->
  api ->
  TmuxProg a ->
  m a
interpret (TmuxState cmds) api (Pure a) = a <$ runCommands api (const $ Right ()) (Cmds cmds)
interpret (TmuxState cmds) api (Free (Read cmd decode next)) = do
  a <- runCommands api decode $ Cmds (cmd : cmds)
  interpret def api (next a)
interpret (TmuxState cmds) api (Free (Write cmd next)) =
  interpret (TmuxState (cmd : cmds)) api (next ())
interpret _ _ (Free (Failed err)) =
  throwHoist err

runTmux ::
  (TmuxApi m api, MonadIO m, MonadDeepError e TmuxError m) =>
  api ->
  TmuxProg a ->
  m a
runTmux = interpret def
