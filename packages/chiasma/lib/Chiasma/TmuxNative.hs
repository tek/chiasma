module Chiasma.TmuxNative where

import Polysemy.Internal.Sing (KnownList)

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Panes (Panes, TmuxPanes)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Effect.TmuxApi (TmuxApi)
import Chiasma.Effect.TmuxClient (NativeTmux, TmuxClient)
import Chiasma.Interpreter.TmuxApi (InterpretApisNative, RestopApisNative, TmuxApis, type (<$>))
import Chiasma.Tmux (withTmux, withTmuxApis, withTmuxApis', withTmuxApis_, withTmux_)

withTmuxApisNative' ::
  ∀ commands r a .
  InterpretApisNative commands r =>
  Member NativeTmux r =>
  Sem (TmuxApis commands CodecError ++ TmuxClient (Const TmuxRequest) (Const [Text]) : r) a ->
  Sem r a
withTmuxApisNative' =
  withTmuxApis' @commands @CodecError

withTmuxApisNative ::
  ∀ commands r .
  KnownList (TmuxApis commands CodecError) =>
  InterpretApisNative commands r =>
  Member NativeTmux r =>
  InterpretersFor (TmuxApis commands CodecError) r
withTmuxApisNative =
  withTmuxApis @commands @CodecError

withTmuxApisNative_ ::
  ∀ commands r .
  KnownList (TmuxApi <$> commands) =>
  RestopApisNative commands r =>
  Member NativeTmux r =>
  InterpretersFor (TmuxApi <$> commands) r
withTmuxApisNative_ =
  withTmuxApis_ @commands @CodecError

withTmuxNative ::
  ∀ command r .
  Members [NativeTmux, NativeCodecE command] r =>
  InterpreterFor (TmuxApi command !! CodecError) r
withTmuxNative =
  withTmux

withTmuxNative_ ::
  ∀ command r .
  Members [NativeTmux, NativeCodecE command, Stop CodecError] r =>
  InterpreterFor (TmuxApi command) r
withTmuxNative_ =
  withTmux_

withPanesNative ::
  ∀ p r .
  Members [NativeTmux, NativeCodecE (Panes p)] r =>
  InterpreterFor (TmuxPanes p !! CodecError) r
withPanesNative =
  withTmuxNative

withPanesNative_ ::
  ∀ p r .
  Members [NativeTmux, NativeCodecE (Panes p), Stop CodecError] r =>
  InterpreterFor (TmuxPanes p) r
withPanesNative_ =
  withTmuxNative_
