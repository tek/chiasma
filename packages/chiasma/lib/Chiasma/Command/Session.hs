module Chiasma.Command.Session where

import Chiasma.Codec.Data.Client (Client (Client))
import Chiasma.Codec.Data.Session (Session (Session))
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.SessionParams (name)
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (ListClients, ListSessions, NewSession, SwitchClient))
import Chiasma.Data.TmuxError (TmuxError (NoClients))
import Chiasma.Data.TmuxId (SessionId)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxApi (Tmux)

sameId :: SessionId -> Session -> Bool
sameId target (Session i _) = target == i

sessions ::
  Member Tmux r =>
  Sem r [Session]
sessions =
  Tmux.send ListSessions

doesSessionExist ::
  Member Tmux r =>
  SessionId ->
  Sem r Bool
doesSessionExist sessionId =
  any (sameId sessionId) <$> sessions

existingSessionId ::
  Member Tmux r =>
  SessionId ->
  Sem r (Maybe SessionId)
existingSessionId sessionId = do
  exists <- doesSessionExist sessionId
  pure $ if exists then Just sessionId else Nothing

newSession ::
  Member Tmux r =>
  Ident ->
  Sem r Session
newSession name =
  Tmux.send (NewSession def { name = Just name })

clientForSession :: SessionId -> [Client] -> Maybe Client
clientForSession session =
  find \case
    Client _ False sid ->
      sid == session
    _ ->
      False

switchClient ::
  Members [Tmux, Stop TmuxError] r =>
  SessionId ->
  SessionId ->
  Sem r ()
switchClient clientSession session = do
  clients <- Tmux.send ListClients
  dbgs (clientSession, session, clients)
  Client client _ _ <- stopNote NoClients (clientForSession clientSession clients)
  Tmux.send (SwitchClient client (Target.Session session))
