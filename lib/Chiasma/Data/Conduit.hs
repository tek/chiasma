{-# LANGUAGE DataKinds #-}

module Chiasma.Data.Conduit(
  createSinkFlush,
) where

import Conduit (ConduitT, Flush)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Conduit.Binary (sinkHandleFlush)
import Data.Conduit.Process.Typed (StreamType(STInput), StreamSpec, createPipe)

createSinkFlush :: MonadIO m => StreamSpec 'STInput (ConduitT (Flush ByteString) o m ())
createSinkFlush = sinkHandleFlush <$> createPipe
