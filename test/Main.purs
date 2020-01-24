module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Example.Parse as Parse
import Example.Run as Run
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Run.test
  Parse.test
