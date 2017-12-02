module Brainfuck.ParserSpec (spec) where

import Test.Hspec
import Brainfuck.Parser

spec :: Spec
spec =
    describe "brainfuck parser" $ do
        it "parses increments and decrements" $ do
            (parse "+") `shouldBe` (Right [Increment])
            (parse " + ") `shouldBe` (Right [Increment])
            (parse "-") `shouldBe` (Right [Decrement])
            (parse " - ") `shouldBe` (Right [Decrement])
        it "parses movements" $ do
            (parse "<") `shouldBe` (Right [MoveLeft])
            (parse " < ") `shouldBe` (Right [MoveLeft])
            (parse ">") `shouldBe` (Right [MoveRight])
            (parse " > ") `shouldBe` (Right [MoveRight])
        it "parses I/O" $ do
            (parse ".") `shouldBe` (Right [Output])
            (parse " . ") `shouldBe` (Right [Output])
            (parse ",") `shouldBe` (Right [Input])
            (parse " , ") `shouldBe` (Right [Input])
        it "parses loops" $ do
            (parse "[]")  `shouldBe` (Right [Loop []])
            (parse "[+]")  `shouldBe` (Right [Loop [Increment]])
            (parse "+[+]") `shouldBe` (Right [Increment, Loop [Increment]])
            (parse "+[+].") `shouldBe` (Right [Increment, Loop [Increment], Output])
            (parse " + [ + ] . ") `shouldBe` (Right [Increment, Loop [Increment], Output])
            (parse " + [ + [ + ] + ] . ") `shouldBe`
                (Right [Increment, Loop [Increment, Loop [Increment], Increment], Output])
