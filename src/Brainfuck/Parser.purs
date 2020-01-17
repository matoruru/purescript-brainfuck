module Brainfuck.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodePoints (indexOf', length)
import Effect.Exception (Error, error)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Token (space)
import Text.Parsing.StringParser (ParseError(..), Parser(..))
import Text.Parsing.StringParser.CodePoints (skipSpaces, string)
import Text.Parsing.StringParser.Combinators (sepEndBy)

data Op
  = IncP
  | DecP
  | IncV
  | DecV
  | Put
  | Get
  | Loop (Array Op)

type Program = Array Op

program :: Parser Program
program = do
  --space
  sepEndBy operation skipSpaces

operation :: Parser Op
operation = simpleOp <|> loop

simpleChar :: Parser String
simpleChar = string ">" <|>
             string "<" <|>
             string "+" <|>
             string "-" <|>
             string "." <|>
             string ","

simpleOp :: Parser Op
simpleOp = build <$> simpleChar
  where
    build ">" = IncP
    build "<" = DecP
    build "+" = IncV
    build "-" = DecV
    build "." = Put
    build  _  = Get

loop :: Parser Op
loop = Loop <$> between (string "[") (string "]") program

--
--loop :: Parser String
--loop = between (string "(") (string ")") parse
