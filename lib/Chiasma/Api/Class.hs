module Chiasma.Api.Class(
  TmuxApi(..),
  DecodeTmuxResponse(..),
) where

import Conduit (ConduitT, Void, Flush)

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.TmuxThunk (Cmds, Cmd, TmuxError)
import Chiasma.Native.StreamParse (TmuxOutputBlock)

class TmuxApi a where
  runCommands ::
    (MonadIO m, MonadError TmuxError m) =>
    a ->
    ([Text] -> Either TmuxDecodeError b) ->
    Cmds ->
    m [b]

  -- this cannot be generalised to 'MonadError', because the only sufficient process-streaming abstraction needs
  -- `MonadUnliftIO` :(
  withTmux ::
    (MonadUnliftIO m, MonadThrow m) =>
    a ->
    (ConduitT (Flush Cmd) Void m () -> ConduitT () TmuxOutputBlock m () -> ExceptT TmuxError m b) ->
    ExceptT TmuxError m b

class DecodeTmuxResponse a where
  decode :: String -> Either TmuxError a
