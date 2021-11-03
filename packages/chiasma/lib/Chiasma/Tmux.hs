module Chiasma.Tmux where

import Polysemy.Internal (hoistSem)
import Polysemy.Internal.Sing (KnownList (singList))
import Polysemy.Internal.Union (hoist, weakenMid)

import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxApi (TmuxApi)
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Interpreter.TmuxApi (InterpretApis (interpretApis), TmuxApis, interpretTmuxApi)

withTmuxApis' ::
  ∀ commands err encode decode resource r a .
  Member (Scoped resource (TmuxClient encode decode)) r =>
  InterpretApis commands err encode decode r =>
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
  Member (Scoped resource (TmuxClient encode decode)) r =>
  InterpretersFor (TmuxApis commands err) r
withTmuxApis =
  scoped . interpretApis @commands @err . insertAfter @(TmuxApis commands err) @(TmuxClient encode decode) @r

withTmux ::
  ∀ command err encode decode resource r .
  Members [Scoped resource (TmuxClient encode decode), Codec command encode decode !! err] r =>
  InterpreterFor (TmuxApi command !! err) r
withTmux =
  scoped . interpretTmuxApi . raiseUnder
