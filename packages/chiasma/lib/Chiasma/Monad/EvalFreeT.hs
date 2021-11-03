module Chiasma.Monad.EvalFreeT where

-- import Control.Monad.Trans.Free (FreeF(..), FreeT(..))

-- import Chiasma.Codec.Decode (DecodeFailure)
-- import Chiasma.Data.Cmd (Cmd(..), Cmds(..))
-- import Chiasma.Data.TmuxError (TmuxError)
-- import Chiasma.Data.TmuxThunk (TmuxThunk(..))

-- newtype CmdBuffer = CmdBuffer [Cmd]

-- instance Default CmdBuffer where
--   def = CmdBuffer def

-- type CommandExec m =
--   (∀ b. (Text -> Either DecodeFailure b) -> Cmds -> m (Either TmuxError [b]))

-- evalFreeF ::
--   Monad m =>
--   CommandExec m ->
--   CmdBuffer ->
--   FreeF TmuxThunk a (FreeT TmuxThunk m a) ->
--   m (Either TmuxError a)
-- evalFreeF _ (CmdBuffer []) (Pure a) =
--   pure (Right a)
-- evalFreeF exec (CmdBuffer cmds) (Pure a) =
--   Right a <$ exec (const $ Right ()) (Cmds cmds)
-- evalFreeF exec (CmdBuffer cmds) (Free (Read cmd decode next)) = do
--   a <- exec decode $ Cmds (cmd : cmds)
--   case a of
--     Right a' -> evalFreeT exec def (next a')
--     Left err -> pure (Left err)
-- evalFreeF exec (CmdBuffer cmds) (Free (Write cmd next)) =
--   evalFreeT exec (CmdBuffer (cmd : cmds)) (next ())
-- evalFreeF exec (CmdBuffer cmds) (Free (Flush next)) =
--   exec (const $ Right ()) (Cmds cmds) >>= \case
--     Right _ -> evalFreeT exec def (next ())
--     Left err -> pure (Left err)
-- evalFreeF _ _ (Free (Failed err)) =
--   pure (Left err)

-- evalFreeT ::
--   Monad m =>
--   CommandExec m ->
--   CmdBuffer ->
--   FreeT TmuxThunk m a ->
--   m (Either TmuxError a)
-- evalFreeT exec s (FreeT ma) = do
--   inner <- ma
--   evalFreeF exec s inner
