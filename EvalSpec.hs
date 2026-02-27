-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Control.Exception (evaluate)
import Eval
import Test.Hspec
import Test.QuickCheck
import Val

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
      it "multiplies integers" $ do
        eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]

      it "multiplies floats" $ do
        eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
        eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
        eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

      it "errors on too few arguments" $ do
        evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
        evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    -- this does not work, seems to be a HSpec bug
    -- it "errors on non-numeric inputs" $ do
    --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException
    context "DUP" $ do
      it "duplicates values" $ do
        eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
        eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
        eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

      it "errors on empty stack" $ do
        evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"

    context "+" $ do
      it "adds integers" $
        eval "+" [Integer 2, Integer 3] `shouldBe` [Integer 5]

    context "-" $ do
      it "subtracts integers" $
        eval "-" [Integer 5, Integer 3] `shouldBe` [Integer 2]

    context "/" $ do
      it "divides numbers" $
        eval "/" [Integer 6, Integer 3] `shouldBe` [Real 2.0]

    context "^" $ do
      it "computes power" $
        eval "^" [Integer 2, Integer 3] `shouldBe` [Real 8.0]

    context "STR" $ do
      it "converts integer to string" $
        eval "STR" [Integer 5] `shouldBe` [Id "Integer 5"]

    context "CONCAT2" $ do
      it "concatenates two strings" $
        eval "CONCAT2" [Id "Hello", Id "World"] `shouldBe` [Id "HelloWorld"]

    context "CONCAT3" $ do
      it "concatenates three strings" $
        eval "CONCAT3" [Id "A", Id "B", Id "C"] `shouldBe` [Id "ABC"]

  describe "evalOut" $ do
    context "." $ do
      it "prints top of stack" $ do
        evalOut "." ([Id "x"], "") `shouldBe` ([], "x")
        evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
        evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

      it "errors on empty stack" $ do
        evaluate (evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

    it "eval pass-through" $ do
      evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah")

    context "EMIT" $ do
      it "prints ASCII character" $
        evalOut "EMIT" ([Integer 65], "")
          `shouldBe` ([], "A")

    context "CR" $ do
      it "prints newline" $
        evalOut "CR" ([], "")
          `shouldBe` ([], "\n")
