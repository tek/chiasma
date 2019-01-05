module Chiasma.Native.Process(
  tmuxProcessConfig,
  nativeTmuxProcess,
) where

import GHC.IO.Exception (ExitCode(ExitSuccess))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Internal (packChars, unpackChars)
import Data.Either.Combinators (mapLeft)
import Data.Foldable (traverse_)
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

tmuxProcessConfig :: FilePath -> [String] -> ProcessConfig () () ()
tmuxProcessConfig socket cmds =
  setStdin (byteStringInput $ cmdBytes cmds) $ proc "tmux" ["-S", socket, "-C", "attach"]

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

nativeTmuxProcess :: MonadIO m => FilePath -> ([String] -> Either TmuxDecodeError a) -> Cmds -> m (Either TmuxError [a])
nativeTmuxProcess socket decode cmds@(Cmds cmds') = do
  let cmdLines = fmap formatCmd cmds'
  liftIO $ print "running tmux process:"
  liftIO $ traverse_ print cmdLines
  (code, out) <- readProcessStdout $ tmuxProcessConfig socket cmdLines
  liftIO $ print out
  return $ handleProcessOutput cmds code decode $ unpackChars out
