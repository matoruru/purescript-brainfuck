module Main where

import Prelude

import Brainfuck (Brainfuck)
import Brainfuck.Eval (run)
import Brainfuck.Parser (parse)
import Brainfuck.Type (Cell(..))
import Effect (Effect)
import Effect.Console (logShow)
import PSCI.Support (eval)

--main :: Effect Unit
--main = do
--
--  run """
--+
--
--++
--
--+++
--
--++++
--
--+>+>>
--
-->>++++
--
--+++++++
--
--++++++++
--
--+++++++++
--
--++++++++++
--
--++++++>++++
--
--++++++++++++
--
--+++++++++++++
--
--+++<<<<<<[>[>>
--
-->>>>+>+<<<<<<<-
--
--]>>>>>>>[<<<<<<<
--
--+>>>>>>>-]<[>++++
--
--++++++[-<-[>>+>+<<
--
--<-]>>>[<<<+>>>-]+<[
--
-->[-]<[-]]>[<<[>>>+<<
--
--<-]>>[-]]<<]>>>[>>+>+
--
--<<<-]>>>[<<<+>>>-]+<[>
--
--[-]<[-]]>[<<+>>[-]]<<<<
--
--<<<]>>>>>[++++++++++++++
--
--+++++++++++++++++++++++++
--
--+++++++++.[-]]++++++++++<[
--
--->-<]>+++++++++++++++++++++
--
--+++++++++++++++++++++++++++.
--
--[-]<<<<<<<<<<<<[>>>+>+<<<<-]>
--
-->>>[<<<<+>>>>-]<-[>>.>.<<<[-]]
--
--<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+
--
-->-]>[<+>-]<<<-]
--"""
--
--  run "-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[->+++<]>+.-[->+++<]>.+[->++<]>.---[----->+<]>-.+++[->+++<]>++.++++++++.+++++.--------.-[--->+<]>--.+[->+++<]>+.++++++++.-[++>---<]>+.[-->+++++++<]>.[----->++<]>+.--[--->+<]>---.+.++++[->+++<]>.+++++++++++++.[-->+++++<]>+++.++++++.------.-[--->++<]>-.+++++.++++++.+++[->+++<]>.+++++++++++++.--.++.-------------.[--->+<]>---.+++[->+++<]>.+++++++++++++.[-->+++++<]>+++.--[->++++<]>-.-----.---------.+++++++++++..+++[->+++<]>.+++++++++.-[->+++++<]>-.-[--->++<]>-.+++++.-[->+++++<]>-.[-->+++++<]>.--[-->+++<]>.---.-------------.>-[--->+<]>--.-[------>+<]>.-[--->+<]>----.---------.+++++++.++++.[++>---<]>."
