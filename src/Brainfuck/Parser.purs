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
import Text.Parsing.Parser.String (char, eof, noneOf, skipSpaces)

parse :: BfString -> Either ParseError Brainfuck
parse = flip runParser parser

parser :: Parser BfString Brainfuck
parser = program <* eof

program :: Parser BfString Brainfuck
program = fix \_ -> do
  skipComment
  result <- many do
              skipComment
              result <- op
              skipComment
              pure result
  skipComment
  pure result

op :: Parser BfString Op
op = pure Next  <$> char '>'
 <|> pure Prev  <$> char '<'
 <|> pure Inc   <$> char '+'
 <|> pure Dec   <$> char '-'
 <|> pure Print <$> char '.'
 <|> pure Read  <$> char ','
 <|> fix \_ -> loop

loop :: Parser BfString Op
loop = Loop <$> between (char '[') (char ']') (fix \_ -> program)

skipComment :: Parser BfString Unit
skipComment = do
  skipSpaces
  skipMany $ noneOf ['>', '<', '+', '-', '.', ',', '[', ']']
