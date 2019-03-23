module Chiasma.Api.Class where

import Conduit (ConduitT, Flush, Void, transPipe)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor (void)
import Data.Text (Text)
import UnliftIO.Exception (throwString)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmd, Cmds)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Native.StreamParse (TmuxOutputBlock)

class TmuxApi m a where
  runCommands ::
    a ->
    ([Text] -> Either TmuxDecodeError b) ->
    Cmds ->
    m [b]

  withTmux ::
    a ->
    (ConduitT (Flush Cmd) Void m () -> ConduitT () TmuxOutputBlock m () -> m b) ->
    m b

-- instance (MonadIO m, TmuxApi m a) => TmuxApi (ExceptT e m) a where
--   runCommands a f = lift . runCommands a f
--   withTmux a f =
--     lift $ withTmux a (\i -> throwLeft <=< g i)
--     where
--       g i o = runExceptT $ f (transPipe lift i) (transPipe lift o)
--       throwLeft :: Either e c -> m c
--       throwLeft = either (const $ throwString "bad stdio conduit") return

class DecodeTmuxResponse a where
  decode :: String -> Either TmuxError a
