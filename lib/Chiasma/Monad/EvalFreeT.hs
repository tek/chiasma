{-# LANGUAGE RankNTypes #-}

module Chiasma.Monad.EvalFreeT(
  evalFreeT,
) where

import Control.Monad.Error.Class (MonadError(throwError), liftEither)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Data.Default.Class (Default(def))
import Data.Text (Text)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.TmuxThunk (Cmd(..), Cmds(..), TmuxThunk(..), TmuxError)

newtype CmdBuffer = CmdBuffer [Cmd]

instance Default CmdBuffer where
  def = CmdBuffer def

type CommandExec m =
  (∀ b. ([Text] -> Either TmuxDecodeError b) -> Cmds -> m (Either TmuxError [b]))

evalFreeF ::
  (Monad m, MonadTrans t, MonadError TmuxError (t m)) =>
  CommandExec m ->
  CmdBuffer ->
  FreeF TmuxThunk a (FreeT TmuxThunk m a) ->
  t m a
evalFreeF _ (CmdBuffer []) (Pure a) =
  return a
evalFreeF exec (CmdBuffer cmds) (Pure a) =
  lift $ a <$ exec (const $ Right ()) (Cmds cmds)
evalFreeF exec (CmdBuffer cmds) (Free (Read cmd decode next)) = do
  a <- lift $ exec decode $ Cmds (cmd : cmds)
  a' <- liftEither a
  evalFreeT exec def (next a')
evalFreeF exec (CmdBuffer cmds) (Free (Write cmd next)) =
  evalFreeT exec (CmdBuffer (cmd : cmds)) (next ())
evalFreeF _ _ (Free (Failed err)) =
  throwError err

evalFreeT ::
  (Monad m, MonadTrans t, MonadError TmuxError (t m)) =>
  CommandExec m ->
  CmdBuffer ->
  FreeT TmuxThunk m a ->
  t m a
evalFreeT exec s (FreeT ma) = do
  inner <- lift ma
  evalFreeF exec s inner
