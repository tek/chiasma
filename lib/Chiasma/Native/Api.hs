module Chiasma.Native.Api(
  TmuxApi(..),
  TmuxNative(..),
) where

import Conduit (ConduitT, Void, (.|), mapC, Flush)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (packChars)
import Data.Conduit.Process.Typed (
  ProcessConfig,
  getStdin,
  getStdout,
  setStdin,
  setStdout,
  withProcess,
  createSource,
  proc,
  )
import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Data.Conduit (createSinkFlush)
import Chiasma.Data.TmuxThunk (Cmd(..), CmdName(..), CmdArgs(..))
import Chiasma.Native.Process (nativeTmuxProcess, socketArg)
import Chiasma.Native.StreamParse (parseConduit)
import Text.ParserCombinators.Parsec ()

newtype TmuxNative =
  TmuxNative { tmuxServerSocket :: Maybe FilePath }

formatCmd :: Cmd -> ByteString
formatCmd (Cmd (CmdName name) (CmdArgs args)) = packChars . unwords $ name : args ++ ["\n"]

tmuxProcessConfig ::
  MonadIO m =>
  Maybe FilePath ->
  ProcessConfig (ConduitT (Flush ByteString) Void m ()) (ConduitT () ByteString m ()) ()
tmuxProcessConfig sock =
  setStdin createSinkFlush $ setStdout createSource $ proc "tmux" $ socketArg sock ++ ["-C", "attach"]

instance TmuxApi TmuxNative where
  runCommands (TmuxNative socket) decode cmds =
    nativeTmuxProcess socket decode cmds

  withTmux (TmuxNative socket) f =
    ExceptT $ withProcess (tmuxProcessConfig socket) (runExceptT . handler)
    where
      handler prc =
        let
          stdin = mapC (fmap formatCmd) .| getStdin prc
          stdout = getStdout prc .| parseConduit
        in f stdin stdout
