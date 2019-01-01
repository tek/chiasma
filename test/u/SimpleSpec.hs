{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SimpleSpec(
  htf_thisModulesTests
) where

-- import Data.Either (isLeft)
import Test.Framework
-- import UnliftIO (try)
-- import Chiasma.Data.TmuxThunk (TmuxError)
-- import Chiasma.Monad.Simple (runTmux)
-- import Chiasma.Monad.Tmux (TmuxProg)
-- import qualified Chiasma.Monad.Tmux as Tmux (read, write)

-- number :: TmuxProg a String
-- number = return "1"

-- prog :: TmuxProg a [String]
-- prog = do
--   a <- number
--   out <- Tmux.read "list-panes" ["-t", "%" ++ a]
--   Tmux.write "new-windowXXX" []
--   return out

-- runProg :: IO [String]
-- runProg = runTmux prog

test_simple :: IO ()
test_simple =
  return ()
  -- result <- try runProg
  -- assertEqual True (isLeft (result :: Either TmuxError [String]))
