module Chiasma.Test.Screenshot where

import qualified Data.ByteString as ByteString (writeFile)
import qualified Data.Text as Text (lines, unlines)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Text.IO (readFile)
import Path (Abs, Dir, File, Path, parent, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Effect.TmuxApi (Tmux)

loadScreenshot :: MonadIO m => Path Abs File -> m (Maybe Text)
loadScreenshot path =
  ifM (doesFileExist path) (Just <$> liftIO (readFile (toFilePath path))) (pure Nothing)

storeScreenshot :: MonadIO m => Path Abs File -> [Text] -> m ()
storeScreenshot path text = do
  createDirIfMissing True (parent path)
  liftIO $ ByteString.writeFile (toFilePath path) (Text.encodeUtf8 (Text.unlines text))

takeScreenshot ::
  Member Tmux r =>
  (Text -> Text) ->
  Int ->
  Sem r [Text]
takeScreenshot sanitize =
  fmap (fmap sanitize) . capturePane . PaneId

recordScreenshot ::
  Members [Tmux, Embed IO] r =>
  (Text -> Text) ->
  Path Abs File ->
  Int ->
  Sem r ()
recordScreenshot sanitize path paneId = do
  current <- takeScreenshot sanitize paneId
  storeScreenshot path current

testScreenshot ::
  Members [Tmux, Embed IO] r =>
  (Text -> Text) ->
  Path Abs File ->
  Int ->
  Sem r (Maybe ([Text], [Text]))
testScreenshot sanitize path pane = do
  current <- takeScreenshot sanitize pane
  loadScreenshot path >>= check current
  where
    check current (Just existing) =
      pure $ Just (current, Text.lines existing)
    check current Nothing =
      Nothing <$ storeScreenshot path current

screenshotSanitized ::
  Members [Tmux, Error Text, Embed IO] r =>
  (Text -> Text) ->
  Bool ->
  Path Abs Dir ->
  Text ->
  Int ->
  Sem r (Maybe ([Text], [Text]))
screenshotSanitized sanitize record storage name paneId = do
  rel <- fromEither (first show (parseRelFile (toString name)))
  let path = storage </> rel
  if record then Nothing <$ recordScreenshot sanitize path paneId else testScreenshot sanitize path paneId

screenshot ::
  Members [Tmux, Error Text, Embed IO] r =>
  Bool ->
  Path Abs Dir ->
  Text ->
  Int ->
  Sem r (Maybe ([Text], [Text]))
screenshot =
  screenshotSanitized id
