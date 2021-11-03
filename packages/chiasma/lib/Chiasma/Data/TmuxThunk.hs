module Chiasma.Data.TmuxThunk where

-- import Chiasma.Codec.Decode (DecodeFailure)
-- import Chiasma.Data.Cmd (Cmd(..))
-- import Chiasma.Data.TmuxError (TmuxError(..))

-- data TmuxThunk next =
--   ∀ a . Read Cmd (Text -> Either DecodeFailure a) ([a] -> next)
--   |
--   Write Cmd (() -> next)
--   |
--   Flush (() -> next)
--   |
--   Failed TmuxError

-- deriving instance Functor TmuxThunk
