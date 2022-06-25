module Chiasma.Data.Panes where

import Data.List.Extra (firstJust)

import Chiasma.Class.CmdArgs (cmdArgs)
import qualified Chiasma.Codec as Codec
import Chiasma.Codec (TmuxCodec, multi, single)
import qualified Chiasma.Data.DecodeError as DecodeFailure
import Chiasma.Data.DecodeError (DecodeError (DecodeError))
import qualified Chiasma.Data.PaneSelection as PaneSelection
import Chiasma.Data.PaneSelection (PaneSelection)
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxQuery (TmuxQuery)
import Chiasma.Data.TmuxRequest (TmuxRequest (TmuxRequest))
import Chiasma.Data.WithPaneId (WithPaneId (WithPaneId))
import Chiasma.Effect.TmuxApi (TmuxApi)

-- |A 'TmuxApi' command for listing panes, with different query criteria.
-- The constructors taking a 'PaneSelection' list all panes that are present in the selected scope, but may constrain
-- the return value.
-- The constructors 'Get' and 'Find' return only the pane with the requested ID.
data Panes (p :: Type) (a :: Type) :: Type where
  -- |Return all panes covered by the selection.
  List :: PaneSelection -> Panes p [p]
  -- |Return one pane covered by the selection, fail if there is none.
  First :: PaneSelection -> Panes p p
  -- |Return one pane covered by the selection, fail if there is none or more than one.
  One :: PaneSelection -> Panes p p
  -- |Return the pane with the specified ID, fail if there is none.
  Get :: PaneId -> Panes p p
  -- |Return the pane with the specified ID if it exists.
  Find :: PaneId -> Panes p (Maybe p)

type TmuxPanes p =
  TmuxApi (Panes p)

query ::
  ∀ p a .
  TmuxCodec p =>
  Panes p a ->
  TmuxQuery
query _ =
  Codec.query @p

selection :: Panes p a -> PaneSelection
selection = \case
  List ps -> ps
  First ps -> ps
  One ps -> ps
  Get i -> PaneSelection.InWindow (Target.Pane i)
  Find _ -> PaneSelection.All

request ::
  Panes p a ->
  TmuxQuery ->
  TmuxRequest
request (selection -> s) =
  TmuxRequest "list-panes" (cmdArgs s) . Just

encode ::
  ∀ p a .
  TmuxCodec p =>
  Panes p a ->
  TmuxRequest
encode (Get i) =
  request (Get i) (Codec.query @(WithPaneId p))
encode (Find i) =
  request (Find i) (Codec.query @(WithPaneId p))
encode cmd =
  request cmd (query cmd)

sameId :: PaneId -> WithPaneId a -> Maybe a
sameId target (WithPaneId paneId p)
  | target == paneId = Just p
  | otherwise = Nothing

paneById ::
  TmuxCodec p =>
  PaneId ->
  [Text] ->
  Either DecodeError (Maybe p)
paneById paneId out = do
  firstJust (sameId paneId) <$> multi out

decode ::
  ∀ p a .
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
  Get paneId -> do
    ps <- multi @(WithPaneId p) out
    maybeToRight (DecodeError out DecodeFailure.TargetMissing) (firstJust (sameId paneId) ps)
  Find paneId -> do
    ps <- multi @(WithPaneId p) out
    pure (firstJust (sameId paneId) ps)
