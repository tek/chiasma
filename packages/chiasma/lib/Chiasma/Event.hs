module Chiasma.Event where

import Exon (exon)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (withAsync_)
import qualified Polysemy.Log as Log
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds (Seconds))
import Prelude hiding (listen)

import Chiasma.Data.Event (Event)
import Chiasma.Data.ReceiveEvent (ReceiveEvent (ReceiveEvent))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Effect.Codec (Codec)
import qualified Chiasma.Effect.TmuxApi as TmuxApi
import Chiasma.Effect.TmuxApi (TmuxApi)
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Tmux (withTmux)

receiveEvent ::
  ∀ r .
  Members [TmuxApi ReceiveEvent, Events Event] r =>
  Sem r ()
receiveEvent =
  Conc.publish =<< TmuxApi.send ReceiveEvent

listenLoop ::
  ∀ err t d r .
  Show err =>
  Members [TmuxApi ReceiveEvent !! err, Events Event, Time t d, Log] r =>
  Sem r ()
listenLoop = do
  resume @_ @(TmuxApi _) receiveEvent \ err ->
    Log.error [exon|Receiving tmux event: #{show err}|]
  listenLoop

listen ::
  ∀ enc dec err t d r .
  Show err =>
  Members [Codec ReceiveEvent enc dec !! err, RunStop] r =>
  Members [Scoped_ (TmuxClient enc dec) !! TmuxError, Events Event, Time t d, Log] r =>
  Sem r ()
listen = do
  resume @_ @(Scoped_ _) (withTmux listenLoop) \ err -> do
    Log.error [exon|Lost connection to tmux: #{show err}
Reconnecting...|]
    Time.sleep (Seconds 1)

withTmuxEvents ::
  Show err =>
  Member (Codec ReceiveEvent enc dec !! err) r =>
  Member (Scoped_ (TmuxClient enc dec) !! TmuxError) r =>
  Members [Events Event, Time t d, RunStop, Log, Race, Async, Bracket] r =>
  Sem r a ->
  Sem r a
withTmuxEvents =
  withAsync_ do
    forever listen
