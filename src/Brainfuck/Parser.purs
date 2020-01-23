module Brainfuck.Parser
  ( parser
  , parse
  ) where

import Prelude hiding (between)

import Brainfuck.Type (Brainfuck, Op(..), BfString)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (many)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (between, skipMany)
import Text.Parsing.Parser.String (char, eof, noneOf)

parse :: BfString -> Either ParseError Brainfuck
parse = flip runParser parser

parser :: Parser BfString Brainfuck
parser = comment *> program <* comment <* eof

program :: Parser BfString Brainfuck
program = fix \_ -> many $ between comment comment op

op :: Parser BfString Op
op = fix \_ ->
     pure Next  <$> char '>'
 <|> pure Prev  <$> char '<'
 <|> pure Inc   <$> char '+'
 <|> pure Dec   <$> char '-'
 <|> pure Print <$> char '.'
 <|> pure Read  <$> char ','
 <|> pure Loop  <*> loop '[' ']'

loop :: Char -> Char -> Parser BfString Brainfuck
loop l r = fix \_ -> between (char l) (char r) program

comment :: Parser BfString Unit
comment = skipMany $ noneOf ['>', '<', '+', '-', '.', ',', '[', ']']
