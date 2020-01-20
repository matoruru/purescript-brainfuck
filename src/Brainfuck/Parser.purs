module Brainfuck.Parser
  ( parser
  , parse
  ) where

import Prelude hiding (between)

import Brainfuck.Type (BF, Op(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (many)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (between, skipMany)
import Text.Parsing.Parser.String (char, eof, noneOf, skipSpaces)

parse :: String -> Either ParseError BF
parse = flip runParser parser

parser :: Parser String BF
parser = program <* eof

program :: Parser String BF
program = fix \_ -> do
  skipComment
  result <- many do
              skipComment
              result <- op
              skipComment
              pure result
  skipComment
  pure result

op :: Parser String Op
op = pure Next  <$> char '>'
 <|> pure Prev  <$> char '<'
 <|> pure Inc   <$> char '+'
 <|> pure Dec   <$> char '-'
 <|> pure Print <$> char '.'
 <|> pure Read  <$> char ','
 <|> fix \_ -> loop

loop :: Parser String Op
loop = Loop <$> between (char '[') (char ']') (fix \_ -> program)

skipComment :: Parser String Unit
skipComment = do
  skipSpaces
  skipMany $ noneOf ['>', '<', '+', '-', '.', ',', '[', ']']
