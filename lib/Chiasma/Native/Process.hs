module Chiasma.Native.Process(
  tmuxProcessConfig,
  nativeTmuxProcess,
) where

import GHC.IO.Exception (ExitCode(ExitSuccess))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Internal (packChars, unpackChars)
import Data.Either.Combinators (mapLeft)
import Data.List (intercalate)
import System.Process.Typed (ProcessConfig, readProcessStdout, proc, setStdin, byteStringInput)
import Chiasma.Data.TmuxThunk (CmdName(..), Cmd(..), CmdArgs(..), Cmds(..), TmuxError(..))
import Chiasma.Native.Parse (resultLines)

cmdBytes :: [String] -> ByteString
cmdBytes cmds = packChars $ intercalate "\n" $ reverse $ "" : cmds

tmuxProcessConfig :: FilePath -> [String] -> ProcessConfig () () ()
tmuxProcessConfig socket cmds =
  setStdin (byteStringInput $ cmdBytes cmds) $ proc "tmux" ["-S", socket, "-C", "attach"]

handleProcessOutput :: Cmds -> ExitCode -> String -> Either TmuxError [[String]]
handleProcessOutput cmds ExitSuccess out = do
  events <- mapLeft (TmuxOutputParsingFailed cmds out) $ resultLines out
  return $ drop 1 events
handleProcessOutput cmds _ out =
  Left $ TmuxProcessFailed cmds out

formatCmd :: Cmd -> String
formatCmd (Cmd (CmdName name) (CmdArgs args)) = unwords $ name : args

nativeTmuxProcess :: MonadIO m => FilePath -> Cmds -> m (Either TmuxError [[String]])
nativeTmuxProcess socket cmds@(Cmds cmds') = do
  let cmdLines = fmap formatCmd cmds'
  (code, out) <- readProcessStdout $ tmuxProcessConfig socket cmdLines
  liftIO $ print out
  return $ handleProcessOutput cmds code $ unpackChars out
