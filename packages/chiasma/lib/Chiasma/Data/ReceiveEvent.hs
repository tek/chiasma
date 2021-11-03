module Chiasma.Data.ReceiveEvent where

import Chiasma.Data.Event (Event)

data ReceiveEvent :: Type -> Type where
  ReceiveEvent :: ReceiveEvent Event
