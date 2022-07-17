module Chiasma.Interpreter.TmuxClient where

import Data.Sequence ((|>))
import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, File, Path, toFilePath)
import Polysemy.Conc (interpretAtomic)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedResumableWith_)
import qualified Polysemy.Log as Log
import qualified Polysemy.Process as Process
import Polysemy.Process (
  OutputPipe (Stderr, Stdout),
  Process,
  SystemProcess,
  interpretProcessInputId,
  interpretProcessOutputLeft,
  interpretProcess_,
  withProcess_,
  )
import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.SystemProcessError (SystemProcessError, SystemProcessScopeError)
import Polysemy.Process.Interpreter.Process (ProcessQueues)
import Polysemy.Process.Interpreter.ProcessOutput (interpretProcessOutputTextLines)
import Polysemy.Process.Interpreter.SystemProcess (PipesProcess, interpretSystemProcessNative_)
import System.Process.Typed (ProcessConfig, proc)

import qualified Chiasma.Data.TmuxError as TmuxError
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxNative (TmuxNative (TmuxNative))
import qualified Chiasma.Data.TmuxOutputBlock as TmuxOutputBlock
import Chiasma.Data.TmuxOutputBlock (TmuxOutputBlock)
import qualified Chiasma.Data.TmuxRequest as TmuxRequest
import Chiasma.Data.TmuxRequest (TmuxRequest (TmuxRequest))
import qualified Chiasma.Effect.TmuxClient as TmuxClient
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Interpreter.ProcessOutput (interpretProcessOutputTmuxBlock)

type TmuxQueues =
  ProcessQueues (Either Text TmuxOutputBlock) Text

type TmuxProc =
  Process ByteString (Either Text TmuxOutputBlock)

validate :: TmuxRequest -> TmuxOutputBlock -> Either TmuxError [Text]
validate request = \case
  TmuxOutputBlock.Success a ->
    Right a
  TmuxOutputBlock.Error a ->
    Left (TmuxError.RequestFailed request a)

tmuxRequest ::
  Members [Process ByteString (Either Text TmuxOutputBlock), Log, Stop TmuxError] r =>
  TmuxRequest ->
  Sem r [Text]
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
  proc (toFilePath exe) (foldMap socketArg socket <> ["-C", "-u", "attach-session", "-f", "ignore-size"])

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
    tmuxRequest (TmuxRequest "refresh-client" ["-C", "10000x10000"] Nothing)
    raiseUnder action <* flush

interpretTmuxProcessBuffered ::
  Members [AtomicState (Seq TmuxRequest), Scoped res TmuxProc !! ProcessError, Log, Embed IO] r =>
  InterpreterFor (Scoped () (TmuxClient (Const TmuxRequest) (Const [Text])) !! TmuxError) r
interpretTmuxProcessBuffered =
  interpretScopedResumableWith_ @'[TmuxProc] tmuxSession \case
    TmuxClient.Schedule (Const request) ->
      atomicModify' (|> request)
    TmuxClient.Send (Const cmd) -> do
      flush
      Const <$> tmuxRequest cmd
{-# inline interpretTmuxProcessBuffered #-}

interpretTmuxWithProcess ::
  Members [Scoped res TmuxProc !! ProcessError, Log, Embed IO] r =>
  InterpreterFor (Scoped () (TmuxClient (Const TmuxRequest) (Const [Text])) !! TmuxError) r
interpretTmuxWithProcess =
  interpretAtomic mempty .
  interpretTmuxProcessBuffered .
  raiseUnder
{-# inline interpretTmuxWithProcess #-}

interpretTmuxNative ::
  ∀ r .
  Members [Reader TmuxNative, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Scoped () (TmuxClient (Const TmuxRequest) (Const [Text])) !! TmuxError) r
interpretTmuxNative =
  interpretSystemProcessTmux .
  interpretProcessTmux .
  interpretTmuxWithProcess .
  raiseUnder2
{-# inline interpretTmuxNative #-}
