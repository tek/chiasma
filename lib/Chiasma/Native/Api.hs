{-# LANGUAGE UndecidableInstances #-}

module Chiasma.Native.Api where

import Chiasma.Data.TmuxError (TmuxError)
import Conduit (ConduitT, Flush, Void, mapC, (.|))
import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as Catch (bracket)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (packChars)
import Data.Conduit.Process.Typed (createSource)
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

import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Data.Cmd (Cmd(..), CmdArgs(..), CmdName(..))
import Chiasma.Data.Conduit (createSinkFlush)
import Chiasma.Native.Process (nativeTmuxProcess, socketArg)
import Chiasma.Native.StreamParse (parseConduit)

newtype TmuxNative =
  TmuxNative { tmuxServerSocket :: Maybe FilePath }
  deriving Show

formatCmd :: Cmd -> ByteString
formatCmd (Cmd (CmdName name) (CmdArgs args)) = packChars . unwords $ name : args ++ ["\n"]

tmuxProcessConfig ::
  MonadIO m =>
  Maybe FilePath ->
  ProcessConfig (ConduitT (Flush ByteString) Void m ()) (ConduitT () ByteString m ()) ()
tmuxProcessConfig sock =
  setStdin createSinkFlush $ setStdout createSource $ proc "tmux" $ socketArg sock ++ ["-C", "attach"]

withProcess :: (MonadIO m, MonadMask m)
            => ProcessConfig stdin stdout stderr
            -> (Process stdin stdout stderr -> m a)
            -> m a
withProcess config = Catch.bracket (startProcess config) stopProcess

instance (MonadIO m, MonadDeepError e TmuxError m, MonadMask m) => TmuxApi m TmuxNative where
  runCommands (TmuxNative socket) =
    nativeTmuxProcess socket

  withTmux (TmuxNative socket) f =
    withProcess (tmuxProcessConfig socket) handler
    where
      handler prc =
        let
          stdin = mapC (fmap formatCmd) .| getStdin prc
          stdout = getStdout prc .| parseConduit
        in f stdin stdout
