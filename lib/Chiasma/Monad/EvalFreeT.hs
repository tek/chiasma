{-# LANGUAGE RankNTypes #-}

module Chiasma.Monad.EvalFreeT(
  evalFreeT,
) where

import Control.Monad.Trans.Free (FreeF(..), FreeT(..))
import Data.Default.Class (Default(def))
import Data.Text (Text)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmd(..), Cmds(..))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk(..))

newtype CmdBuffer = CmdBuffer [Cmd]

instance Default CmdBuffer where
  def = CmdBuffer def

type CommandExec m =
  (∀ b. ([Text] -> Either TmuxDecodeError b) -> Cmds -> m (Either TmuxError [b]))

evalFreeF ::
  Monad m =>
  CommandExec m ->
  CmdBuffer ->
  FreeF TmuxThunk a (FreeT TmuxThunk m a) ->
  m (Either TmuxError a)
evalFreeF _ (CmdBuffer []) (Pure a) =
  return (Right a)
evalFreeF exec (CmdBuffer cmds) (Pure a) =
  Right a <$ exec (const $ Right ()) (Cmds cmds)
evalFreeF exec (CmdBuffer cmds) (Free (Read cmd decode next)) = do
  a <- exec decode $ Cmds (cmd : cmds)
  case a of
    Right a' -> evalFreeT exec def (next a')
    Left err -> return (Left err)
evalFreeF exec (CmdBuffer cmds) (Free (Write cmd next)) =
  evalFreeT exec (CmdBuffer (cmd : cmds)) (next ())
evalFreeF _ _ (Free (Failed err)) =
  return (Left err)

evalFreeT ::
  Monad m =>
  CommandExec m ->
  CmdBuffer ->
  FreeT TmuxThunk m a ->
  m (Either TmuxError a)
evalFreeT exec s (FreeT ma) = do
  inner <- ma
  evalFreeF exec s inner
