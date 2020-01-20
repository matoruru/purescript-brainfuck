module Brainfuck.Type
  ( BF
  , Op(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)

type BF = List Op

data Op
  = Next
  | Prev
  | Inc
  | Dec
  | Print
  | Read
  | Loop BF

derive instance genericOp :: Generic Op _
instance showOp :: Show Op where
  show s = genericShow s
