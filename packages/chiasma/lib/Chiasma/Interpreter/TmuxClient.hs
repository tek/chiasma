module Chiasma.Interpreter.TmuxClient where

import qualified Conc
import Conc (Consume, Lock, interpretAtomic, interpretLockReentrant, lock, withAsync_)
import Data.Sequence ((|>))
import qualified Data.Text as Text
import Exon (exon)
import qualified Log as Log
import Path (Abs, File, Path, relfile, toFilePath)
import Polysemy.Conc.Interpreter.Mask (interpretMaskFinal)
import Polysemy.Process.Interpreter.Process (ProcessQueues)
import qualified Process as Process
import Process (
  OutputPipe (Stderr, Stdout),
  Process,
  ProcessError,
  SystemProcess,
  SystemProcessError,
  SystemProcessScopeError,
  interpretProcessInputId,
  interpretProcessOutputLeft,
  interpretProcessOutputTextLines,
  interpretProcess_,
  interpretSystemProcessNative_,
  resolveExecutable,
  withProcess_,
  )
import System.Process.Typed (ProcessConfig, proc)

import qualified Chiasma.Data.TmuxError as TmuxError
import Chiasma.Data.TmuxError (TmuxError (NoExe))
import qualified Chiasma.Data.TmuxEvent as TmuxEvent
import Chiasma.Data.TmuxEvent (TmuxEvent)
import Chiasma.Data.TmuxNative (TmuxNative (TmuxNative))
import Chiasma.Data.TmuxNotification (TmuxNotification (..))
import qualified Chiasma.Data.TmuxOutputBlock as TmuxOutputBlock
import Chiasma.Data.TmuxOutputBlock (TmuxOutputBlock)
import qualified Chiasma.Data.TmuxRequest as TmuxRequest
import Chiasma.Data.TmuxRequest (TmuxRequest (TmuxRequest))
import Chiasma.Data.TmuxResponse (TmuxResponse (TmuxResponse))
import qualified Chiasma.Effect.TmuxClient as TmuxClient
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Interpreter.ProcessOutput (interpretProcessOutputTmuxEvent)

type TmuxQueues =
  ProcessQueues (Either Text TmuxEvent) Text

type TmuxProc =
  Process ByteString (Either Text TmuxEvent)

validate :: TmuxRequest -> TmuxOutputBlock -> Either TmuxError TmuxResponse
validate request = \case
  TmuxOutputBlock.Success a ->
    Right (TmuxResponse a)
  TmuxOutputBlock.Error a ->
    Left (TmuxError.RequestFailed request a)

-- | Send a command and wait for the response.
-- The lock ensures only one request is in flight at a time (tmux control mode responses are ordered).
-- Opens a fresh subscription so only messages arriving after the send are seen.
tmuxRequest ::
  Members [TmuxProc, EventConsumer TmuxEvent, Lock, Log, Stop TmuxError] r =>
  TmuxRequest ->
  Sem r TmuxResponse
tmuxRequest request =
  lock $ subscribe do
    Log.trace [exon|tmux request: #{Text.stripEnd (decodeUtf8 cmdline)}|]
    Process.send cmdline
    stopEither =<< Conc.consumeFirstJust \case
      TmuxEvent.Response block -> pure (Just (validate request block))
      TmuxEvent.Notification _ -> pure Nothing
  where
    cmdline =
      TmuxRequest.encode request

socketArg :: Path Abs File -> [String]
socketArg socket =
  ["-S", toFilePath socket]

tmuxProc ::
  TmuxNative ->
  ProcessConfig () () ()
tmuxProc (TmuxNative exe socket) =
  proc (toFilePath exe) (foldMap socketArg socket <> ["-C", "-u", "attach-session", "-f", "ignore-size,no-output"])

interpretSystemProcessTmux ::
  Members [Reader TmuxNative, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped_ (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r
interpretSystemProcessTmux sem = do
  conf <- tmuxProc <$> ask
  interpretSystemProcessNative_ conf sem

interpretProcessTmux ::
  Member (Scoped_ (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped_ TmuxProc !! ProcessError) r
interpretProcessTmux sem = do
  interpretProcessOutputTmuxEvent @'Stdout $
    interpretProcessOutputTextLines @'Stderr $
    interpretProcessOutputLeft @'Stderr $
    interpretProcessInputId $
    interpretProcess_ def $
    insertAt @1 sem

-- | Consume messages from the process until the first command response block.
-- Tmux emits a response block for the implicit attach-session command on connect.
-- Notification lines are discarded during this phase.
drainInitial ::
  Members [TmuxProc, Log] r =>
  Sem r ()
drainInitial =
  Process.recv >>= \case
    Right (TmuxEvent.Response _) ->
      Log.trace "tmux: drained initial response"
    Right (TmuxEvent.Notification n) -> do
      Log.trace [exon|tmux: initial notification: #{n.name}|]
      drainInitial
    Left err -> do
      Log.warn [exon|tmux: initial recv error: #{err}|]
      drainInitial

-- | Background receiver loop: reads from the tmux process and publishes each message.
receiverLoop ::
  Members [Events TmuxEvent, TmuxProc, Log] r =>
  Sem r ()
receiverLoop =
  forever do
    Process.recv >>= \case
      Left err ->
        Log.warn [exon|tmux recv error: #{err}|]
      Right msg -> do
        Log.trace [exon|tmux recv message: #{show msg}|]
        Conc.publish msg

-- | Send all scheduled requests and discard their responses.
flush ::
  Members [EventConsumer TmuxEvent, TmuxProc, AtomicState (Seq TmuxRequest), Lock, Log, Stop TmuxError] r =>
  Sem r ()
flush =
  traverse_ tmuxRequest =<< atomicState' (mempty,)

tmuxSession ::
  ∀ r a .
  Members [Scoped_ TmuxProc !! ProcessError, AtomicState (Seq TmuxRequest), Stop TmuxError] r =>
  Members [Lock, Log, Resource, Race, Async, Embed IO] r =>
  Sem (Consume TmuxEvent : EventConsumer TmuxEvent : TmuxProc : r) a ->
  Sem r a
tmuxSession action =
  resumeHoist TmuxError.ProcessFailed $ withProcess_ do
    drainInitial
    Conc.interpretEventsChan do
      withAsync_ receiverLoop do
        subscribe do
          void $ tmuxRequest (TmuxRequest "refresh-client" ["-C", "200x1000"] Nothing)
          subsume_ action <* flush

interpretTmuxProcessBuffered ::
  Members [AtomicState (Seq TmuxRequest), Scoped_ TmuxProc !! ProcessError] r =>
  Members [Lock, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped_ (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxProcessBuffered =
  interpretScopedResumableWith_ @'[Consume TmuxEvent, EventConsumer TmuxEvent, TmuxProc] (const tmuxSession) \case
    TmuxClient.Schedule request ->
      atomicModify' (|> request)
    TmuxClient.Send cmd -> do
      flush
      tmuxRequest cmd
    TmuxClient.ReceiveNotification ->
      Conc.consumeFirstJust $ pure . \case
        TmuxEvent.Notification n -> Just n
        TmuxEvent.Response _ -> Nothing

interpretTmuxWithProcess ::
  Members [Scoped_ TmuxProc !! ProcessError, Log, Resource, Race, Async, Final IO, Embed IO] r =>
  InterpreterFor (Scoped_ (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxWithProcess =
  interpretAtomic mempty .
  interpretMaskFinal .
  interpretLockReentrant .
  interpretTmuxProcessBuffered .
  raiseUnder3

interpretTmuxNative ::
  ∀ r .
  Members [Reader TmuxNative, Log, Resource, Race, Async, Final IO, Embed IO] r =>
  InterpreterFor (Scoped_ (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxNative =
  interpretSystemProcessTmux .
  interpretProcessTmux .
  interpretTmuxWithProcess .
  raiseUnder2

interpretTmuxFailing ::
  TmuxError ->
  InterpreterFor (Scoped_ (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxFailing err =
  interpretScopedResumable_ mempty \ () -> \case
    TmuxClient.Schedule _ ->
      stop err
    TmuxClient.Send _ ->
      stop err
    TmuxClient.ReceiveNotification ->
      stop err

withTmuxNativeEnv ::
  Member (Embed IO) r =>
  Maybe (Path Abs File) ->
  (Maybe TmuxNative -> Sem r a) ->
  Sem r a
withTmuxNativeEnv socket use =
  use . fmap (flip TmuxNative socket) . rightToMaybe =<< resolveExecutable [relfile|tmux|] Nothing

runReaderTmuxNativeEnv ::
  Members [Error TmuxError, Embed IO] r =>
  Maybe (Path Abs File) ->
  InterpreterFor (Reader TmuxNative) r
runReaderTmuxNativeEnv socket sem = do
  tn <- withTmuxNativeEnv socket (note NoExe)
  runReader tn sem

interpretTmuxNativeEnv ::
  Members [Error TmuxError, Log, Resource, Race, Async, Final IO, Embed IO] r =>
  Maybe (Path Abs File) ->
  InterpreterFor (Scoped_ (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxNativeEnv socket =
  runReaderTmuxNativeEnv socket . interpretTmuxNative . raiseUnder

interpretTmuxNativeEnvGraceful ::
  Members [Log, Resource, Race, Async, Final IO, Embed IO] r =>
  Maybe (Path Abs File) ->
  InterpreterFor (Scoped_ (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxNativeEnvGraceful socket sem =
  withTmuxNativeEnv socket \case
    Just tn -> runReader tn (interpretTmuxNative (raiseUnder sem))
    Nothing -> interpretTmuxFailing NoExe sem

interpretTmuxClientNull ::
  InterpreterFor (Scoped_ (TmuxClient i ()) !! TmuxError) r
interpretTmuxClientNull =
  interpretScopedResumable_ mempty \ () -> \case
    TmuxClient.Schedule _ ->
      unit
    TmuxClient.Send _ ->
      unit
    TmuxClient.ReceiveNotification ->
      pure (TmuxNotification {name = "null", args = []})
