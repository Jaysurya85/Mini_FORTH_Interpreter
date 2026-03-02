import Eval
import Interpret
import Test.Hspec
import Val

main :: IO ()
main = hspec $ do
  describe "interpret" $ do
    context "basic RPN" $ do
      it "multiplies two integers" $ do
        interpret "2 3 *"
          `shouldBe` ([Integer 6], "")

      it "multiplies floats and integers" $ do
        interpret "2 2.2 3.4 * *"
          `shouldBe` ([Real 14.960001], "")

    context "printing" $ do
      it "computes product and outputs" $ do
        interpret "2 6 * ."
          `shouldBe` ([], "12")

    context "user defined words (bonus)" $ do
      it "defines and calls SQUARE" $ do
        interpret ": SQUARE DUP * ; 5 SQUARE ."
          `shouldBe` ([], "25")

      it "supports nested user defined words" $ do
        interpret ": SQUARE DUP * ; : FOURTH SQUARE SQUARE ; 2 FOURTH ."
          `shouldBe` ([], "16")
