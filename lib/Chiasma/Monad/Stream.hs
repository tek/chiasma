module Chiasma.Monad.Stream(
  TmuxProg,
  runTmux,
) where

import Conduit (ConduitT, Void, (.|), runConduit, Flush(..), yield, yieldMany)
import qualified Data.Conduit.Combinators as Conduit (drop, head)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Free (FreeT(..))
import Data.Default.Class (Default(def))
import Data.Either.Combinators (mapLeft)
import Chiasma.Api.Class (TmuxApi(..))
import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.TmuxThunk (Cmd(..), Cmds(..), TmuxThunk(..), TmuxError)
import qualified Chiasma.Data.TmuxThunk as TmuxError (TmuxError(ProcessFailed, DecodingFailed))
import Chiasma.Monad.EvalFreeT (evalFreeT)

type TmuxProg = FreeT TmuxThunk

type WriteCmd m =
  ConduitT (Flush Cmd) Void m ()

type ReadOutput m =
  ConduitT () [String] m ()

handleProcessOutput ::
  Cmds ->
  ([String] -> Either TmuxDecodeError a) ->
  Maybe [String] ->
  Either TmuxError [a]
handleProcessOutput cmds decode (Just output) =
  traverse decode' output
  where
    decode' = mapLeft (TmuxError.DecodingFailed cmds (unlines output)) . decode . words
handleProcessOutput cmds _ _ =
  Left $ TmuxError.ProcessFailed cmds "tmux terminated before all commands were processed"

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
    readOutput .| (Conduit.drop (length cmds - 1) >> Conduit.head)
  return $ handleProcessOutput cs decode output

runTmuxProg ::
  (MonadIO m, MonadTrans t, MonadError TmuxError (t m)) =>
  TmuxProg m a ->
  WriteCmd m ->
  ReadOutput m ->
  t m a
runTmuxProg prog writeCmd readOutput = do
  lift $ runConduit $ readOutput .| Conduit.drop 1
  evalFreeT exec def prog
  where
    exec = executeCommands writeCmd readOutput

runTmux ::
  (MonadUnliftIO m, MonadThrow m, TmuxApi api) =>
  api ->
  TmuxProg m a ->
  m (Either TmuxError a)
runTmux api prog =
  runExceptT $ withTmux api $ runTmuxProg prog
