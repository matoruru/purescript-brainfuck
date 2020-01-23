module Test.Main where

import Prelude

import Brainfuck.Eval (eval, run)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  case eval """++++ [++++++++++]""" of
    (Left l) -> logShow l
    (Right r) -> logShow r
  case eval """++++ [++++++++++]""" of
    (Left l) -> logShow l
    (Right r) -> logShow r
