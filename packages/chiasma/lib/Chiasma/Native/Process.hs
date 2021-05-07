module Chiasma.Native.Process where

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Lazy.Internal (unpackChars)
import qualified Data.Text as Text (intercalate, lines, pack, unwords)
import System.Exit (ExitCode(ExitSuccess))
import System.Process.Typed (ProcessConfig, byteStringInput, proc, readProcessStdout, setStdin)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmd(..), CmdArgs(..), CmdName(..), Cmds(..))
import Chiasma.Data.TmuxError (TmuxError)
import qualified Chiasma.Data.TmuxError as TmuxError (
  TmuxError(OutputParsingFailed, NoOutput, ProcessFailed, DecodingFailed),
  )
import Chiasma.Native.Parse (resultLines)

cmdBytes :: [Text] -> Lazy.ByteString
cmdBytes cmds = encodeUtf8 $ Text.intercalate "\n" $ reverse $ "" : cmds

socketArg :: Maybe FilePath -> [Text]
socketArg (Just socket) = ["-S", toText socket]
socketArg Nothing = []

tmuxProcessConfig :: Maybe FilePath -> [Text] -> ProcessConfig () () ()
tmuxProcessConfig socket cmds =
  cons args
  where
  cons =
    setStdin (byteStringInput $ cmdBytes cmds) . proc "tmux"
  args =
    toString <$> (socketArg socket <> ["-C", "attach"])

handleProcessOutput :: Cmds -> ExitCode -> (Text -> Either TmuxDecodeError a) -> Text -> Either TmuxError [a]
handleProcessOutput cmds ExitSuccess decode out = do
  outputs <- mapLeft (TmuxError.OutputParsingFailed cmds (Text.lines out)) $ resultLines out
  case reverse outputs of
    output : _ -> traverse decode' output
    _ -> Left $ TmuxError.NoOutput cmds
  where
    decode' = mapLeft (TmuxError.DecodingFailed cmds out) . decode
handleProcessOutput cmds _ _ out =
  Left $ TmuxError.ProcessFailed cmds out

formatCmd :: Cmd -> Text
formatCmd (Cmd (CmdName name) (CmdArgs args)) =
  Text.unwords $ name : args

nativeTmuxProcess ::
  (MonadIO m, MonadDeepError e TmuxError m) =>
  Maybe FilePath ->
  (Text -> Either TmuxDecodeError a) ->
  Cmds ->
  m [a]
nativeTmuxProcess socket decode cmds@(Cmds cmds') = do
  let cmdLines = fmap formatCmd cmds'
  (code, out) <- readProcessStdout $ tmuxProcessConfig socket cmdLines
  hoistEither $ handleProcessOutput cmds code decode $ Text.pack $ unpackChars out
