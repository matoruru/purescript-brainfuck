module Brainfuck.Type
  ( BfString
  , Brainfuck
  , Op(..)
  , Cell
  , unwrap
  , wrap
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)

type BfString = String

type Brainfuck = List Op

data Op
  = Next
  | Prev
  | Inc
  | Dec
  | Print
  | Read
  | Loop Brainfuck

derive instance genericOp :: Generic Op _
instance showOp :: Show Op where
  show s = genericShow s

derive instance eqOp :: Eq Op

data Cell = Cell Int

derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where
  show = genericShow

unwrap :: Cell -> Int
unwrap (Cell v) = v

wrap :: Int -> Cell
wrap v = Cell $ v `mod` 256
