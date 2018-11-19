module CalculatorSpec (spec) where

import           Calculator (calculator)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Calculator" $ do
    it "" $ do
      calculator "1+1" `shouldBe` "2.0"
      calculator "1-1" `shouldBe` "0.0"
      calculator "1*1" `shouldBe` "1.0"
      calculator "1/1" `shouldBe` "1.0"
