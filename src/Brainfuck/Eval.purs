module Eval where

import Prelude

import Control.Monad.State (State, modify_, put, runState)

newtype Tape = Tape
  { prev :: Array Int
  , curr :: Int
  , next :: Array Int
  }

inc :: Tape -> State Tape Unit
inc (Tape t) = put $ Tape { prev: [], curr: 0, next: [] }
  where
    newprev = tape.prev
    newprev = tape.prev

test :: State Tape Tape
test = do
  inc
  inc

eval :: State Tape Tape
eval = runState test 0
