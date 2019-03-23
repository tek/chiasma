{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Monad.Tmux(
  read,
  write,
  unsafeReadOne,
  readFirst,
  unsafeReadFirst,
  readRaw,
) where

import Control.Monad.Free (liftF)
import Control.Monad.Free.Class (MonadFree)
import Data.Text (Text)
import qualified Data.Text as T (unwords)
import Prelude hiding (read)

import Chiasma.Codec (TmuxCodec, TmuxQuery(unQ))
import qualified Chiasma.Codec as TmuxCodec (TmuxCodec(decode, query))
import Chiasma.Data.Cmd (cmd)
import Chiasma.Data.TmuxError (TmuxError(InvalidOutput))
import Chiasma.Data.TmuxThunk (TmuxThunk(..))

read :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => String -> [String] -> m [a]
read name args =
  liftF $ Read (cmd name (args ++ formatArgs)) TmuxCodec.decode id
  where
    formatArgs = ["-F", "'", unQ (TmuxCodec.query @a), "'"]

unsafeReadOne :: ∀ a m . (TmuxCodec a, MonadFree TmuxThunk m) => String -> [String] -> m a
unsafeReadOne name args = do
  outputs <- read name args
  case outputs of
    [a] -> return a
    [] -> liftF $ Failed $ InvalidOutput "no data" (name ++ unwords args)
    _ -> liftF $ Failed $ InvalidOutput "multiple outputs for `unsafeReadOne`" (name ++ unwords args)

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

readRaw :: ∀ m . (MonadFree TmuxThunk m) => String -> [String] -> m [Text]
readRaw name args =
  liftF $ Read (cmd name args) (Right . T.unwords) id

write :: MonadFree TmuxThunk m => String -> [String] -> m ()
write name args = liftF $ Write (cmd name args) id
