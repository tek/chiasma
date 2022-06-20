module Chiasma.Tmux where

import Polysemy.Internal (hoistSem)
import Polysemy.Internal.Sing (KnownList (singList))
import Polysemy.Internal.Union (hoist, weakenMid)

import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxApi (TmuxApi)
import Chiasma.Effect.TmuxClient (TmuxClient, ScopedTmux)
import Chiasma.Interpreter.TmuxApi (
  InterpretApis (interpretApis),
  RestopApis (restopApis),
  TmuxApis,
  interpretTmuxApi,
  type (<$>),
  )

withTmuxApis' ::
  ∀ commands err encode decode resource r a .
  InterpretApis commands err encode decode r =>
  Member (ScopedTmux resource encode decode) r =>
  Sem (TmuxApis commands err ++ TmuxClient encode decode : r) a ->
  Sem r a
withTmuxApis' =
  scoped . interpretApis @commands @err

insertAfter ::
  ∀ left e r a .
  KnownList left =>
  Sem (left ++ r) a ->
  Sem (left ++ e : r) a
insertAfter =
  hoistSem $ hoist (insertAfter @left @e @r) . weakenMid @r (singList @left) (singList @'[e])

withTmuxApis ::
  ∀ commands err encode decode resource r .
  KnownList (TmuxApis commands err) =>
  InterpretApis commands err encode decode r =>
  Member (ScopedTmux resource encode decode) r =>
  InterpretersFor (TmuxApis commands err) r
withTmuxApis =
  scoped .
  interpretApis @commands @err .
  insertAfter @(TmuxApis commands err) @(TmuxClient encode decode) @r

withTmuxApis_ ::
  ∀ commands err encode decode resource apis r .
  apis ~ TmuxApi <$> commands =>
  KnownList apis =>
  RestopApis commands err encode decode r =>
  Member (ScopedTmux resource encode decode) r =>
  InterpretersFor apis r
withTmuxApis_ =
  scoped .
  restopApis @commands @err .
  insertAfter @apis @(TmuxClient encode decode) @r

withTmux ::
  ∀ command err encode decode resource r .
  Members [ScopedTmux resource encode decode, Codec command encode decode !! err] r =>
  InterpreterFor (TmuxApi command !! err) r
withTmux =
  scoped . interpretTmuxApi . raiseUnder

withTmux_ ::
  ∀ command err encode decode resource r .
  Members [ScopedTmux resource encode decode, Codec command encode decode !! err, Stop err] r =>
  InterpreterFor (TmuxApi command) r
withTmux_ =
  scoped .
  interpretTmuxApi .
  raiseUnder .
  restop @err @(TmuxApi command) .
  raiseUnder
