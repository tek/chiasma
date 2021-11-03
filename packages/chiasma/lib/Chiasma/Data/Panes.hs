module Chiasma.Data.Panes where

import Chiasma.Class.CmdArgs (cmdArgs)
import qualified Chiasma.Codec as Codec
import Chiasma.Codec (TmuxCodec, multi, single)
import qualified Chiasma.Data.DecodeError as DecodeFailure
import Chiasma.Data.DecodeError (DecodeError (DecodeError))
import Chiasma.Data.PaneSelection (PaneSelection)
import Chiasma.Data.TmuxQuery (TmuxQuery)
import Chiasma.Data.TmuxRequest (TmuxRequest (TmuxRequest))
import Chiasma.Effect.TmuxApi (TmuxApi)

data Panes (p :: Type) (a :: Type) :: Type where
  List :: PaneSelection -> Panes p [p]
  First :: PaneSelection -> Panes p p
  One :: PaneSelection -> Panes p p

type TmuxPanes c =
  TmuxApi (Panes c)

query ::
  ∀ p a .
  TmuxCodec p =>
  Panes p a ->
  Maybe TmuxQuery
query _ =
  Just (Codec.query @p)

selection :: Panes p a -> PaneSelection
selection = \case
  List ps -> ps
  First ps -> ps
  One ps -> ps

request ::
  Panes p a ->
  Maybe TmuxQuery ->
  TmuxRequest
request (selection -> s) =
    TmuxRequest "list-panes" (cmdArgs s)

encode ::
  TmuxCodec p =>
  Panes p a ->
  TmuxRequest
encode cmd =
  request cmd (query cmd)

decode ::
  TmuxCodec p =>
  [Text] ->
  Panes p a ->
  Either DecodeError a
decode out = \case
  List _ ->
    multi out
  First _ ->
    multi out >>= \case
      [] -> Left (DecodeError out DecodeFailure.TooFewFields)
      a : _ -> Right a
  One _ ->
    single out
