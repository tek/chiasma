module Chiasma.Monad.Tmux(
  Chiasma.Monad.Tmux.read,
  write,
  TmuxProg,
) where

import Control.Monad.Free (Free, liftF)
import Chiasma.Api.Class (DecodeTmuxResponse(..))
import Chiasma.Data.TmuxThunk (TmuxThunk(..), cmd)

type TmuxProg a = Free (TmuxThunk a)

read :: DecodeTmuxResponse a => String -> [String] -> TmuxProg a [a]
read name args = liftF $ Read (cmd name args) decode id

write :: String -> [String] -> TmuxProg a ()
write name args = liftF $ Write (cmd name args) id
