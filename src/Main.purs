module Main where

import Brainfuck.Parser
import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.StringParser (runParser)

main :: Effect Unit
main = do
  getResult $ runParser simpleChar "a>>>>+++"
  where
    getResult (Right r) = log "success!"
    getResult (Left r)  = log "fail!"
