module Chiasma.Data.WithPaneId where

import qualified Data.Text as Text

import Chiasma.Codec (TmuxCodec (decode, query), trim)
import Chiasma.Codec.Decode (primDecode)
import Chiasma.Data.DecodeError (DecodeError (DecodeError), DecodeFailure (TooFewFields))
import qualified Chiasma.Data.TmuxId as TmuxId
import Chiasma.Data.TmuxId (HasPaneId, PaneId)
import Chiasma.Data.TmuxQuery (TmuxQuery (TmuxQuery))

data WithPaneId a =
  WithPaneId {
    paneId :: PaneId,
    pane :: a
  }
  deriving stock (Eq, Show, Generic)

instance HasPaneId (WithPaneId a) where
  paneId = (.paneId)

safeBreakOn :: Text -> Text -> Maybe (Text, Text)
safeBreakOn n = \case
  "" -> Nothing
  t -> Just (second (Text.drop 1) (Text.breakOn n t))

instance TmuxCodec a => TmuxCodec (WithPaneId a) where
  decode payload =
    case safeBreakOn " " (trim payload) of
      Just (idField, rest) -> do
        pane <- decode rest
        paneId <- first (DecodeError [payload]) (primDecode idField)
        pure WithPaneId {..}
      Nothing ->
        Left (DecodeError [payload] TooFewFields)

  query =
    let TmuxQuery paneQuery = query @a
    in TmuxQuery ("#{pane_id} " <> paneQuery)
