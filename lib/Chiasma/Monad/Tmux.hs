{-# LANGUAGE ScopedTypeVariables #-}

module Chiasma.Monad.Tmux(
  Chiasma.Monad.Tmux.read,
  write,
  TmuxProg,
) where

import Control.Monad.Free (Free, liftF)
import Chiasma.Codec (TmuxCodec, TmuxQuery(unQ))
import qualified Chiasma.Codec as TmuxCodec (TmuxCodec(decode, query))
import Chiasma.Data.TmuxThunk (TmuxThunk(..), cmd)

type TmuxProg = Free TmuxThunk

read :: ∀ a . TmuxCodec a => String -> [String] -> TmuxProg [a]
read name args =
  liftF $ Read (cmd name (args ++ ["-F", "'" ++ unQ (TmuxCodec.query @a) ++ "'"])) TmuxCodec.decode id

write :: String -> [String] -> TmuxProg ()
write name args = liftF $ Write (cmd name args) id
