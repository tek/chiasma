{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Monad.Tmux(
  read,
  write,
  readOne,
  readFirst,
  unsafeReadFirst,
  readRaw,
) where

import Prelude hiding (read)
import Control.Monad.Free (liftF)
import Control.Monad.Free.Class (MonadFree)
import Chiasma.Codec (TmuxCodec, TmuxQuery(unQ))
import qualified Chiasma.Codec as TmuxCodec (TmuxCodec(decode, query))
import Chiasma.Data.TmuxThunk (TmuxThunk(..), cmd, TmuxError(InvalidOutput))

read :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => String -> [String] -> m [a]
read name args =
  liftF $ Read (cmd name (args ++ ["-F", "'" ++ unQ (TmuxCodec.query @a) ++ "'"])) TmuxCodec.decode id

readOne :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => String -> [String] -> m a
readOne name args = do
  outputs <- read name args
  case outputs of
    [a] -> return a
    [] -> liftF $ Failed $ InvalidOutput "no data" (name ++ unwords args)
    _ -> liftF $ Failed $ InvalidOutput "multiple outputs for `readOne`" (name ++ unwords args)

readFirst :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => String -> [String] -> m (Maybe a)
readFirst name args = do
  outputs <- read name args
  return $ case outputs of
    (a : _) -> Just a
    [] -> Nothing

unsafeReadFirst :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => String -> [String] -> m a
unsafeReadFirst name args = do
  mayFirst <- readFirst name args
  case mayFirst of
    (Just a) -> return a
    Nothing -> liftF $ Failed $ InvalidOutput "no data" (name ++ unwords args)

readRaw :: ∀ m . (MonadFree TmuxThunk m) => String -> [String] -> m [String]
readRaw name args =
  liftF $ Read (cmd name args) (Right . unlines) id

write :: MonadFree TmuxThunk m => String -> [String] -> m ()
write name args = liftF $ Write (cmd name args) id
