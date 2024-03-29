module Chiasma.Monad.Tmux where

-- import Control.Monad.Free (liftF)
-- import Control.Monad.Free.Class (MonadFree)

-- import Chiasma.Codec (TmuxCodec, TmuxQuery(unQ))
-- import qualified Chiasma.Codec as TmuxCodec (TmuxCodec(decode, query))
-- import Chiasma.Data.Cmd (cmd)
-- import Chiasma.Data.TmuxError (TmuxError(InvalidOutput))
-- import Chiasma.Data.TmuxThunk (TmuxThunk(..))

-- read :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => Text -> [Text] -> m [a]
-- read name args =
--   liftF $ Read (cmd name (args <> formatArgs)) TmuxCodec.decode id
--   where
--     formatArgs = ["-F", "'", unQ (TmuxCodec.query @a), "'"]

-- unsafeReadOne :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => Text -> [Text] -> m a
-- unsafeReadOne name args = do
--   outputs <- read name args
--   case outputs of
--     [a] -> pure a
--     [] -> liftF $ Failed $ InvalidOutput "no data" (name <> unwords args)
--     _ -> liftF $ Failed $ InvalidOutput "multiple outputs for `unsafeReadOne`" (name <> unwords args)

-- readFirst :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => Text -> [Text] -> m (Maybe a)
-- readFirst name args = do
--   outputs <- read name args
--   pure $ case outputs of
--     (a : _) -> Just a
--     [] -> Nothing

-- unsafeReadFirst :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => Text -> [Text] -> m a
-- unsafeReadFirst name args = do
--   mayFirst <- readFirst name args
--   case mayFirst of
--     (Just a) -> pure a
--     Nothing -> liftF $ Failed $ InvalidOutput "no data" (name <> unwords args)

-- readRaw :: ∀ m . (MonadFree TmuxThunk m) => Text -> [Text] -> m [Text]
-- readRaw name args =
--   liftF $ Read (cmd name args) Right id

-- write :: MonadFree TmuxThunk m => Text -> [Text] -> m ()
-- write name args = liftF $ Write (cmd name args) id

-- flush :: MonadFree TmuxThunk m => m ()
-- flush =
--   liftF $ Flush id
