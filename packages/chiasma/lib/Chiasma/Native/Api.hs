{-# LANGUAGE UndecidableInstances #-}

module Chiasma.Native.Api where

import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Data.Cmd (Cmd(..), CmdArgs(..), CmdName(..))
import Chiasma.Data.Conduit (createSinkFlush)
import Chiasma.Data.TmuxError (TmuxError)
import Conduit (ConduitT, Flush, mapC, (.|))
import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as Catch (bracket)
import Control.Monad.Catch (try)
import Data.Conduit.Process.Typed (createSource)
import qualified Data.Text as Text (unwords)
import System.Process.Typed (
  Process,
  ProcessConfig,
  getStdin,
  getStdout,
  proc,
  setStdin,
  setStdout,
  startProcess,
  stopProcess,
  )
import Text.ParserCombinators.Parsec ()

import Chiasma.Native.Process (nativeTmuxProcess, socketArg)
import Chiasma.Native.StreamParse (parseConduit)
import Control.Monad.Trans.Control (MonadBaseControl)

newtype TmuxNative =
  TmuxNative { tmuxServerSocket :: Maybe FilePath }
  deriving Show

formatCmd :: Cmd -> ByteString
formatCmd (Cmd (CmdName name) (CmdArgs args)) =
  encodeUtf8 . Text.unwords $ name : args ++ ["\n"]

tmuxProcessConfig ::
  MonadIO m =>
  Maybe FilePath ->
  ProcessConfig (ConduitT (Flush ByteString) Void m ()) (ConduitT () ByteString m ()) ()
tmuxProcessConfig sock =
  cons args
  where
    cons =
      setStdin createSinkFlush . setStdout createSource . proc "tmux"
    args =
      toString <$> (socketArg sock ++ ["-C", "attach"])

withProcess :: (MonadIO m, MonadMask m)
            => ProcessConfig stdin stdout stderr
            -> (Process stdin stdout stderr -> m a)
            -> m a
withProcess config = Catch.bracket (startProcess config) (try @_ @SomeException . stopProcess)

instance (MonadBaseControl IO m, MonadIO m, MonadDeepError e TmuxError m, MonadMask m) => TmuxApi m TmuxNative where
  runCommands (TmuxNative socket) =
    nativeTmuxProcess socket

  withTmux (TmuxNative socket) f =
    withProcess (tmuxProcessConfig socket) handler
    where
      handler prc =
        let
          stdin' = mapC (fmap formatCmd) .| getStdin prc
          stdout' = getStdout prc .| parseConduit
        in f stdin' stdout'
