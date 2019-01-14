module Chiasma.Native.Process(
  tmuxProcessConfig,
  nativeTmuxProcess,
  socketArg,
) where

import GHC.IO.Exception (ExitCode(ExitSuccess))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Internal (packChars, unpackChars)
import Data.Either.Combinators (mapLeft)
import Data.List (intercalate)
import System.Process.Typed (ProcessConfig, readProcessStdout, proc, setStdin, byteStringInput)
import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.TmuxThunk (CmdName(..), Cmd(..), CmdArgs(..), Cmds(..), TmuxError(..))
import qualified Chiasma.Data.TmuxThunk as TmuxError (
  TmuxError(OutputParsingFailed, NoOutput, ProcessFailed, DecodingFailed)
  )
import Chiasma.Native.Parse (resultLines)

cmdBytes :: [String] -> ByteString
cmdBytes cmds = packChars $ intercalate "\n" $ reverse $ "" : cmds

socketArg :: Maybe FilePath -> [String]
socketArg (Just socket) = ["-S", socket]
socketArg Nothing = []

tmuxProcessConfig :: Maybe FilePath -> [String] -> ProcessConfig () () ()
tmuxProcessConfig socket cmds =
  setStdin (byteStringInput $ cmdBytes cmds) $ proc "tmux" $ socketArg socket ++ ["-C", "attach"]

handleProcessOutput :: Cmds -> ExitCode -> ([String] -> Either TmuxDecodeError a) -> String -> Either TmuxError [a]
handleProcessOutput cmds ExitSuccess decode out = do
  outputs <- mapLeft (TmuxError.OutputParsingFailed cmds (lines out)) $ resultLines out
  case reverse outputs of
    output : _ -> traverse decode' output
    _ -> Left $ TmuxError.NoOutput cmds
  where
    decode' = mapLeft (TmuxError.DecodingFailed cmds out) . decode . words
handleProcessOutput cmds _ _ out =
  Left $ TmuxError.ProcessFailed cmds out

formatCmd :: Cmd -> String
formatCmd (Cmd (CmdName name) (CmdArgs args)) = unwords $ name : args

nativeTmuxProcess ::
  (MonadIO m, MonadError TmuxError m) =>
  Maybe FilePath ->
  ([String] -> Either TmuxDecodeError a) ->
  Cmds ->
  m [a]
nativeTmuxProcess socket decode cmds@(Cmds cmds') = do
  let cmdLines = fmap formatCmd cmds'
  (code, out) <- readProcessStdout $ tmuxProcessConfig socket cmdLines
  liftEither $ handleProcessOutput cmds code decode $ unpackChars out
