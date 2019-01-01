module Chiasma.Monad.Tmux(
  Chiasma.Monad.Tmux.read,
  write,
  TmuxProg,
) where

import Control.Monad.Free (Free, liftF)
import Chiasma.Data.TmuxThunk (TmuxThunk(..), cmd)

type TmuxProg a = Free (TmuxThunk a)

read :: String -> [String] -> TmuxProg a [[String]]
read name args = liftF $ Read (cmd name args) id

write :: String -> [String] -> TmuxProg a ()
write name args = liftF $ Write (cmd name args) id
