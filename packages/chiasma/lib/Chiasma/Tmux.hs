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
import Chiasma.Data.Panes (Panes, TmuxPanes)

withTmuxApis' ::
  ∀ commands err i o resource r a .
  InterpretApis commands err i o r =>
  Member (ScopedTmux resource i o) r =>
  Sem (TmuxApis commands err ++ TmuxClient i o : r) a ->
  Sem r a
withTmuxApis' =
  scoped_ . interpretApis @commands @err

insertAfter ::
  ∀ left e r a .
  KnownList left =>
  Sem (left ++ r) a ->
  Sem (left ++ e : r) a
insertAfter =
  hoistSem $ hoist (insertAfter @left @e @r) . weakenMid @r (singList @left) (singList @'[e])

withTmuxApis ::
  ∀ commands err i o resource r .
  KnownList (TmuxApis commands err) =>
  InterpretApis commands err i o r =>
  Member (ScopedTmux resource i o) r =>
  InterpretersFor (TmuxApis commands err) r
withTmuxApis =
  scoped_ .
  interpretApis @commands @err .
  insertAfter @(TmuxApis commands err) @(TmuxClient i o) @r

withTmuxApis_ ::
  ∀ commands err i o resource apis r .
  apis ~ TmuxApi <$> commands =>
  KnownList apis =>
  RestopApis commands err i o r =>
  Member (ScopedTmux resource i o) r =>
  InterpretersFor apis r
withTmuxApis_ =
  scoped_ .
  restopApis @commands @err .
  insertAfter @apis @(TmuxClient i o) @r

withTmux ::
  ∀ command err i o resource r .
  Members [ScopedTmux resource i o, Codec command i o !! err] r =>
  InterpreterFor (TmuxApi command !! err) r
withTmux =
  scoped_ . interpretTmuxApi . raiseUnder

withTmux_ ::
  ∀ command err i o resource r .
  Members [ScopedTmux resource i o, Codec command i o !! err, Stop err] r =>
  InterpreterFor (TmuxApi command) r
withTmux_ =
  scoped_ .
  interpretTmuxApi .
  raiseUnder .
  restop @err @(TmuxApi command) .
  raiseUnder

withPanes ::
  ∀ p err i o resource r .
  Members [ScopedTmux resource i o, Codec (Panes p) i o !! err] r =>
  InterpreterFor (TmuxPanes p !! err) r
withPanes =
  withTmux

withPanes_ ::
  ∀ p err i o resource r .
  Members [ScopedTmux resource i o, Codec (Panes p) i o !! err, Stop err] r =>
  InterpreterFor (TmuxPanes p) r
withPanes_ =
  withTmux_
