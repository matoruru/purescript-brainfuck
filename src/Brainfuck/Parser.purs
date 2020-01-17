module Brainfuck.Parser where

import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (string)

data Op
  = IncP
  | DecP
  | IncV
  | DecV
  | Put
  | Get
  | Loop (Array Op)

type Program = Array Op

simpleChar :: Parser String
simpleChar = string ">" <|>
             string "<" <|>
             string "+" <|>
             string "-" <|>
             string "." <|>
             string ","

--operation :: Parser Op
--operation = simpleChar
--
--loop :: Parser String
--loop = between (string "(") (string ")") parse
