module Test.Main where

import Test.Main.Sanity (sanityTests)
import Test.Main.Local (localIsos)
import Test.Main.Protocol (protocolTests)

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)


main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] do
  sanityTests
  localIsos
  protocolTests

