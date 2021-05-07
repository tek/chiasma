{-# LANGUAGE DataKinds #-}

module Chiasma.Data.Conduit where

import Conduit (ConduitT, Flush)
import Data.Conduit.Binary (sinkHandleFlush)
import Data.Conduit.Process.Typed (StreamSpec, StreamType(STInput), createPipe)

createSinkFlush :: MonadIO m => StreamSpec 'STInput (ConduitT (Flush ByteString) o m ())
createSinkFlush = sinkHandleFlush <$> createPipe
