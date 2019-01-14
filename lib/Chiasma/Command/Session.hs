module Chiasma.Command.Session(
  sessions,
  doesSessionExist,
  newSession,
  existingSessionId,
) where

import Control.Monad.Free.Class (MonadFree)
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Codec.Data (Session(Session))
import Chiasma.Data.TmuxId (SessionId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Monad.Tmux as Tmux (read, readOne)

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
  Tmux.readOne "new-session" ["-s", identString name, "-P"]
