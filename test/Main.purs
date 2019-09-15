module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Spec (describe, pending, it)
import Test.Spec.Runner (runSpec)
import Test.Reporter.Console (consoleReporter)


main :: Effect Unit
main = do
  log "You should add some tests."
