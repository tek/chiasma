module Chiasma.Test.CaptureTest where

import Polysemy.Test (UnitTest, assertEq)

import Chiasma.Command.Pane (capturePane, sendKeys)
import Chiasma.Data.SendKeysParams (Key (Lit))
import qualified Chiasma.Data.TmuxError as TmuxError
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Effect.TmuxClient (ScopedTmux)
import Chiasma.Test.Tmux (tmuxTest)
import Chiasma.Test.Wait (assertWait)
import Chiasma.Tmux (withTmux)

test_capture :: UnitTest
test_capture =
  tmuxTest do
    restop @TmuxError @(ScopedTmux _ _ _) do
      withTmux do
        resumeHoist TmuxError.codec do
          assertWait (capturePane 0) (assertEq ["$"])
          sendKeys 0 [Lit "echo 1"]
          assertWait (capturePane 0) (assertEq ["$ echo 1", "1", "$"])
