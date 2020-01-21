module Brainfuck.Eval where

import Prelude

import Brainfuck (Brainfuck, parse)
import Brainfuck.Type (Cell, Op(..), unwrap, wrap)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (State, modify_, put, runState)
import Data.Array (drop, head, last)
import Data.Char as Char
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)

incCell :: Cell -> Cell
incCell = wrap <<< (_ + 1) <<< unwrap

decCell :: Cell -> Cell
decCell = wrap <<< (_ - 1) <<< unwrap

getNext :: Array Cell -> Cell
getNext = head >>> case _ of
  Nothing -> wrap 0
  Just v  -> v

-- getPrev :: Array Cell -> Either String Cell
-- getPrev = last >>> case _ of
--   Nothing -> Left "[Brainfuck.Eval.getPrev]: Out of range!"
--   Just c  -> Right $ Cell v
getPrev :: Array Cell -> Cell
getPrev = last >>> case _ of
  Nothing -> wrap 0
  Just c  -> c

fromCharCode :: Int -> String
fromCharCode = Char.fromCharCode >>> case _ of
  Nothing -> mempty
  Just c  -> singleton c

cellToString :: Cell -> String
cellToString = fromCharCode <<< unwrap

newtype Machine = Machine
  { display :: String
  , tape    :: Tape
  }

instance showMachine :: Show Machine where
  show (Machine m) = "(Machine { display: " <> "\""<> m.display <> "\""
                          <>  ", tape: "    <> show m.tape
                          <> " })"

newtype Tape = Tape
  { prev :: Array Cell
  , curr :: Cell
  , next :: Array Cell
  }

instance showTape :: Show Tape where
  show (Tape t) = "(Tape { prev: " <> show t.prev
                    <>  ", curr: " <> show t.curr
                    <>  ", next: " <> show t.next
                    <> " })"

exec :: Op -> State Machine Unit
exec = case _ of
  Next    -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev <> [ t.curr ]
                            , curr: t.next # getNext
                            , next: t.next # drop 1
                            }
            }
  Prev    -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev <> [ t.curr ]
                            , curr: t.next # getNext
                            , next: t.next # drop 1
                            }
            }
  Inc     -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev
                            , curr: t.curr # incCell
                            , next: t.next
                            }
            }
  Dec     -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev
                            , curr: t.curr # decCell
                            , next: t.next
                            }
            }
  Print   -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display <> cellToString t.curr
            , tape   : Tape { prev: t.prev
                            , curr: t.curr
                            , next: t.next
                            }
            }
  Read    -> modify_ identity
  Loop bf -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    if unwrap t.curr /= 0
    then Machine m # eval bf
    else Machine m

init :: Machine
init = Machine { display: mempty, tape: Tape { prev: [], curr: wrap 0, next: [] } }

eval :: Brainfuck -> Machine -> Machine
eval Nil   machine = machine
eval (h:t) machine = case runState (exec h) machine of
                       Tuple _ next -> eval t next

run :: String -> Effect Unit
run = parse >>> case _ of
  Left  l -> logShow l
  Right r -> logShow $ eval r init