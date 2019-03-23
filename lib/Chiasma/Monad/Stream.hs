module Chiasma.Monad.Stream where

import Conduit (ConduitT, Flush(..), Void, runConduit, sinkList, yield, yieldMany, (.|))
import Control.Monad ((<=<))
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Free (FreeT(..))
import qualified Data.Conduit.Combinators as Conduit (drop, take)
import Data.Default.Class (Default(def))
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T (words)

import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmd(..), Cmds(..))
import Chiasma.Data.TmuxError (TmuxError)
import qualified Chiasma.Data.TmuxError as TmuxError (TmuxError(ProcessFailed, DecodingFailed, CommandFailed))
import Chiasma.Data.TmuxThunk (TmuxThunk(..))
import Chiasma.Monad.EvalFreeT (evalFreeT)
import Chiasma.Native.StreamParse (TmuxOutputBlock)
import qualified Chiasma.Native.StreamParse as TmuxOutputBlock (TmuxOutputBlock(..))

type TmuxProg = FreeT TmuxThunk

type WriteCmd m =
  ConduitT (Flush Cmd) Void m ()

type ReadOutput m =
  ConduitT () TmuxOutputBlock m ()

handleProcessOutput ::
  Cmds ->
  ([Text] -> Either TmuxDecodeError a) ->
  [TmuxOutputBlock] ->
  Either TmuxError [a]
handleProcessOutput cs@(Cmds cmds) _ output | length output < length cmds =
  Left $ TmuxError.ProcessFailed cs "tmux terminated before all commands were processed"
handleProcessOutput cmds decode output = do
  readOutput <- foldl validate (Right []) output
  traverse decode' readOutput
  where
    validate (Left err) _ = Left err
    validate _ (TmuxOutputBlock.Success a) = Right a
    validate _ (TmuxOutputBlock.Error a) = Left $ TmuxError.CommandFailed cmds a
    decode' outputLine = mapLeft (TmuxError.DecodingFailed cmds outputLine) $ decode $ T.words outputLine

executeCommands ::
  MonadIO m =>
  WriteCmd m ->
  ReadOutput m ->
  ([Text] -> Either TmuxDecodeError a) ->
  Cmds ->
  m (Either TmuxError [a])
executeCommands writeCmd readOutput decode cs@(Cmds cmds) = do
  output <- runConduit $ do
    yieldMany (Chunk <$> reverse cmds) .| writeCmd
    yield Flush .| writeCmd
    readOutput .| Conduit.take (length cmds) .| sinkList
  return $ handleProcessOutput cs decode output

runTmuxProg ::
  MonadIO m =>
  TmuxProg m a ->
  WriteCmd m ->
  ReadOutput m ->
  m (Either TmuxError a)
runTmuxProg prog writeCmd readOutput = do
  runConduit $ readOutput .| Conduit.drop 1
  evalFreeT (executeCommands writeCmd readOutput) def prog

runTmuxE ::
  (MonadIO m, TmuxApi m api) =>
  api ->
  TmuxProg m a ->
  m (Either TmuxError a)
runTmuxE api prog =
  withTmux api (runTmuxProg prog)

runTmux ::
  (MonadIO m, MonadDeepError e TmuxError m, TmuxApi m api) =>
  api ->
  TmuxProg m a ->
  m a
runTmux api =
  hoistEither <=< runTmuxE api
