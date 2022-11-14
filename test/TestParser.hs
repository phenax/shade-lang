module TestParser where

import Test.Hspec

test :: SpecWith ()
test = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      1 `shouldBe` 1
