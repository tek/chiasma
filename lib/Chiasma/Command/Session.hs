module Chiasma.Command.Session where

import Control.Monad.Free.Class (MonadFree)

import Chiasma.Codec.Data (Session(Session))
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.TmuxId (SessionId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Monad.Tmux as Tmux (read, unsafeReadOne, write)

sameId :: SessionId -> Session -> Bool
sameId target (Session i) = target == i

sessions :: MonadFree TmuxThunk m => m [Session]
sessions =
  Tmux.read "list-sessions" []

doesSessionExist :: MonadFree TmuxThunk m => SessionId -> m Bool
doesSessionExist sessionId =
  any (sameId sessionId) <$> sessions

existingSessionId :: MonadFree TmuxThunk m => SessionId -> m (Maybe SessionId)
existingSessionId sessionId = do
  exists <- doesSessionExist sessionId
  return $ if exists then Just sessionId else Nothing

newSession :: MonadFree TmuxThunk m => Ident -> m Session
newSession name =
  Tmux.unsafeReadOne "new-session" ["-s", identText name, "-P"]

activateSession :: MonadFree TmuxThunk m => Int -> m ()
activateSession sessionId =
  Tmux.write "send-keys" ["-t", "%1", "'tmux switch-client -t \\$" <> show sessionId <> "'", "enter"]
