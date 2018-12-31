module Chiasma.Monad.Tmux(
  Chiasma.Monad.Tmux.read,
  write,
  TmuxProg,
) where

import Control.Monad.Free (Free, liftF)
import Chiasma.Data.TmuxThunk (TmuxThunk(..), Cmd(..), Args(..))

type TmuxProg a = Free (TmuxThunk a)

read :: String -> [String] -> TmuxProg a [String]
read cmd args = liftF $ Read (Cmd cmd) (Args args) id

write :: String -> [String] -> TmuxProg a ()
write cmd args = liftF $ Write (Cmd cmd) (Args args) id
