module Chiasma.Monad.Stream(
  TmuxProg,
  runTmux,
) where

import Conduit (ConduitT, Void, (.|), runConduit, Flush(..), yield, yieldMany, sinkList)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Free (FreeT(..))
import qualified Data.Conduit.Combinators as Conduit (drop, take)
import Data.Default.Class (Default(def))
import Data.Either.Combinators (mapLeft)

import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.TmuxThunk (Cmd(..), Cmds(..), TmuxThunk(..), TmuxError)
import qualified Chiasma.Data.TmuxThunk as TmuxError (TmuxError(ProcessFailed, DecodingFailed, CommandFailed))
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
  ([String] -> Either TmuxDecodeError a) ->
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
    decode' outputLine = mapLeft (TmuxError.DecodingFailed cmds outputLine) $ decode $ words outputLine

executeCommands ::
  MonadIO m =>
  WriteCmd m ->
  ReadOutput m ->
  ([String] -> Either TmuxDecodeError a) ->
  Cmds ->
  m (Either TmuxError [a])
executeCommands writeCmd readOutput decode cs@(Cmds cmds) = do
  output <- runConduit $ do
    yieldMany (Chunk <$> reverse cmds) .| writeCmd
    yield Flush .| writeCmd
    readOutput .| Conduit.take (length cmds) .| sinkList
  return $ handleProcessOutput cs decode output

runTmuxProg ::
  (MonadIO m, MonadTrans t, MonadError TmuxError (t m)) =>
  TmuxProg m a ->
  WriteCmd m ->
  ReadOutput m ->
  t m a
runTmuxProg prog writeCmd readOutput = do
  lift $ runConduit $ readOutput .| Conduit.drop 1
  evalFreeT (executeCommands writeCmd readOutput) def prog

runTmux ::
  (MonadUnliftIO m, MonadThrow m, TmuxApi api) =>
  api ->
  TmuxProg m a ->
  m (Either TmuxError a)
runTmux api prog =
  runExceptT $ withTmux api $ runTmuxProg prog
