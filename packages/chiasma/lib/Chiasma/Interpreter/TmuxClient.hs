module Chiasma.Interpreter.TmuxClient where

import Conc (interpretAtomic, interpretScopedResumableWith_, interpretScopedResumable_)
import Data.Sequence ((|>))
import qualified Data.Text as Text
import Exon (exon)
import qualified Log as Log
import Path (Abs, File, Path, relfile, toFilePath)
import Polysemy.Process.Interpreter.Process (ProcessQueues)
import qualified Process as Process
import Process (
  OutputPipe (Stderr, Stdout),
  PipesProcess,
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
import Chiasma.Data.TmuxNative (TmuxNative (TmuxNative))
import qualified Chiasma.Data.TmuxOutputBlock as TmuxOutputBlock
import Chiasma.Data.TmuxOutputBlock (TmuxOutputBlock)
import qualified Chiasma.Data.TmuxRequest as TmuxRequest
import Chiasma.Data.TmuxRequest (TmuxRequest (TmuxRequest))
import Chiasma.Data.TmuxResponse (TmuxResponse (TmuxResponse))
import qualified Chiasma.Effect.TmuxClient as TmuxClient
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Interpreter.ProcessOutput (interpretProcessOutputTmuxBlock)

type TmuxQueues =
  ProcessQueues (Either Text TmuxOutputBlock) Text

type TmuxProc =
  Process ByteString (Either Text TmuxOutputBlock)

validate :: TmuxRequest -> TmuxOutputBlock -> Either TmuxError TmuxResponse
validate request = \case
  TmuxOutputBlock.Success a ->
    Right (TmuxResponse a)
  TmuxOutputBlock.Error a ->
    Left (TmuxError.RequestFailed request a)

tmuxRequest ::
  Members [Process ByteString (Either Text TmuxOutputBlock), Log, Stop TmuxError] r =>
  TmuxRequest ->
  Sem r TmuxResponse
tmuxRequest request = do
  Log.trace [exon|tmux request: #{Text.stripEnd (decodeUtf8 cmdline)}|]
  Process.send cmdline
  Process.recv >>= \case
    Left err -> stop (TmuxError.RequestFailed request [err])
    Right block -> do
      Log.trace [exon|tmux response: #{show block}|]
      stopEither (validate request block)
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
  proc (toFilePath exe) (foldMap socketArg socket <> ["-C", "-u", "attach-session"])

interpretSystemProcessTmux ::
  Members [Reader TmuxNative, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped PipesProcess (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r
interpretSystemProcessTmux sem = do
  conf <- tmuxProc <$> ask
  interpretSystemProcessNative_ conf sem

interpretProcessTmux ::
  Member (Scoped res (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped () TmuxProc !! ProcessError) r
interpretProcessTmux sem = do
  interpretProcessOutputTmuxBlock @'Stdout $
    interpretProcessOutputTextLines @'Stderr $
    interpretProcessOutputLeft @'Stderr $
    interpretProcessInputId $
    interpretProcess_ def $
    insertAt @1 sem
{-# inline interpretProcessTmux #-}

flush ::
  Members [TmuxProc, AtomicState (Seq TmuxRequest), Log, Stop TmuxError] r =>
  Sem r ()
flush =
  traverse_ tmuxRequest =<< atomicState' (mempty,)

tmuxSession ::
  ∀ res r a .
  Members [Scoped res TmuxProc !! ProcessError, AtomicState (Seq TmuxRequest), Log, Stop TmuxError] r =>
  Sem (TmuxProc : r) a ->
  Sem r a
tmuxSession action =
  resumeHoist @ProcessError @(Scoped res TmuxProc) TmuxError.ProcessFailed $ withProcess_ do
    void Process.recv
    raiseUnder action <* flush

interpretTmuxProcessBuffered ::
  Members [AtomicState (Seq TmuxRequest), Scoped res TmuxProc !! ProcessError, Log, Embed IO] r =>
  InterpreterFor (Scoped () (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxProcessBuffered =
  interpretScopedResumableWith_ @'[TmuxProc] tmuxSession \case
    TmuxClient.Schedule request ->
      atomicModify' (|> request)
    TmuxClient.Send cmd -> do
      flush
      tmuxRequest cmd
{-# inline interpretTmuxProcessBuffered #-}

interpretTmuxWithProcess ::
  Members [Scoped res TmuxProc !! ProcessError, Log, Embed IO] r =>
  InterpreterFor (Scoped () (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxWithProcess =
  interpretAtomic mempty .
  interpretTmuxProcessBuffered .
  raiseUnder
{-# inline interpretTmuxWithProcess #-}

interpretTmuxNative ::
  ∀ r .
  Members [Reader TmuxNative, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped () (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxNative =
  interpretSystemProcessTmux .
  interpretProcessTmux .
  interpretTmuxWithProcess .
  raiseUnder2
{-# inline interpretTmuxNative #-}

interpretTmuxFailing ::
  TmuxError ->
  InterpreterFor (Scoped () (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxFailing err =
  interpretScopedResumable_ (pure ()) \ () -> \case
    TmuxClient.Schedule _ ->
      stop err
    TmuxClient.Send _ ->
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
{-# inline runReaderTmuxNativeEnv #-}

interpretTmuxNativeEnv ::
  Members [Error TmuxError, Log, Resource, Race, Async, Embed IO] r =>
  Maybe (Path Abs File) ->
  InterpreterFor (Scoped () (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxNativeEnv socket =
  runReaderTmuxNativeEnv socket . interpretTmuxNative . raiseUnder
{-# inline interpretTmuxNativeEnv #-}

interpretTmuxNativeEnvGraceful ::
  Members [Log, Resource, Race, Async, Embed IO] r =>
  Maybe (Path Abs File) ->
  InterpreterFor (Scoped () (TmuxClient TmuxRequest TmuxResponse) !! TmuxError) r
interpretTmuxNativeEnvGraceful socket sem =
  withTmuxNativeEnv socket \case
    Just tn -> runReader tn (interpretTmuxNative (raiseUnder sem))
    Nothing -> interpretTmuxFailing NoExe sem
{-# inline interpretTmuxNativeEnvGraceful #-}

interpretTmuxClientNull ::
  InterpreterFor (Scoped () (TmuxClient i ()) !! TmuxError) r
interpretTmuxClientNull =
  interpretScopedResumable_ (pure ()) \ () -> \case
    TmuxClient.Schedule _ ->
      unit
    TmuxClient.Send _ ->
      unit
{-# inline interpretTmuxClientNull #-}
