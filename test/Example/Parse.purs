module Example.Parse ( test )where

import Prelude

import Brainfuck (Op(..), parse)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (ParseError(..))
import Text.Parsing.Parser.Pos (Position(..))

test :: Spec Unit
test =
  describe "Parse" do
    it "empty string" do
      shouldEqual
        (parse "")
        (Right Nil)

    it "space string" do
      shouldEqual
        (parse " ")
        (Right Nil)

    it "empty heredoc" do
      shouldEqual
        (parse """""")
        (Right Nil)

    it "space heredoc" do
      shouldEqual
        (parse """ """)
        (Right Nil)

    it "empty loop" do
      shouldEqual
        (parse "[]")
        (Right ((Loop Nil) : Nil))

    it "comment and error 1" do
      shouldEqual
        (parse """ abc  + > ] abc """)
        (Left (ParseError "Expected EOF" (Position { line: 1, column: 11 })))

    it "comment and error 2" do
      shouldEqual
        (parse """ abc  + > [ abc """)
        (Left (ParseError "Expected '['" (Position { line: 1, column: 17 })))
