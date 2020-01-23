module Brainfuck.Eval where

import Prelude

import Brainfuck (Brainfuck, parse)
import Brainfuck.Type (Cell, Op(..), BfString, unwrap, wrap)
import Control.Monad.State (State, execState, modify_)
import Data.Array (cons, drop, dropEnd, head, last, snoc)
import Data.Char as Char
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Text.Parsing.Parser (ParseError)

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
  Next -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev `snoc` t.curr
                            , curr: t.next # getNext
                            , next: t.next # drop 1
                            }
            }
  Prev -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev # dropEnd 1
                            , curr: t.prev # getPrev
                            , next: t.next `flip cons` t.curr
                            }
            }
  Inc -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev
                            , curr: t.curr # incCell
                            , next: t.next
                            }
            }
  Dec -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display
            , tape   : Tape { prev: t.prev
                            , curr: t.curr # decCell
                            , next: t.next
                            }
            }
  Print -> modify_ \(Machine m) -> m.tape # \(Tape t) -> do
    Machine { display: m.display <> cellToString t.curr
            , tape   : Tape { prev: t.prev
                            , curr: t.curr
                            , next: t.next
                            }
            }
  Read -> modify_ identity

  loop@(Loop bf) -> modify_ \mac@(Machine m) -> m.tape # \(Tape t) -> do
    go t mac where
      go t' mac'
        | unwrap t'.curr == 0 = mac'
        | otherwise           = evalImpl (loop : Nil) $ evalImpl bf mac'

init :: Machine
init = Machine { display: mempty, tape: Tape { prev: [], curr: wrap 0, next: [] } }

eval :: BfString -> Either ParseError Machine
eval = flip evalWith init

evalWith :: BfString -> Machine -> Either ParseError Machine
evalWith bs machine = case parse bs of
  Left  l -> Left l
  Right r -> Right $ evalImpl r machine

evalImpl :: Brainfuck -> Machine -> Machine
evalImpl Nil   machine = machine
evalImpl (h:t) machine = execState (exec h) machine # evalImpl t

run :: BfString -> Either ParseError String
run = parse >>> case _ of
  Left  l -> Left l
  Right r -> Right $ evalImpl r init # \(Machine m) -> m.display
