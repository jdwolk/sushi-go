module SushiGo.CardsSpec (
  spec
) where

import           SushiGo.Cards
import           Test.Hspec

spec :: Spec
spec = do
  describe "score" $ do
    describe "for sashimi" $ do
      context "3x" $ do
        it "scores 10 points" $ do
          score [Sashimi, Sashimi, Sashimi] `shouldBe` 10
      context "2x" $ do
        it "scores 0 points" $ do
          score [Sashimi, Sashimi] `shouldBe` 0

  describe "group" $ do
    describe"for sashimi" $ do
      it "makes even groups of 3 sashimi" $ do
        group [Sashimi, Sashimi, Sashimi] `shouldBe` GroupResult [SashimiGroup] []
      it "adds extras to remainder" $ do
        group [Sashimi, Sashimi, Sashimi, Sashimi] `shouldBe` GroupResult [SashimiGroup] [Sashimi]

    describe "for tempura" $ do
      it "makes even groups of 2 tempura" $ do
        group [Tempura, Tempura] `shouldBe` GroupResult [TempuraGroup] []
      it "adds extras to remainder" $ do
        group [Tempura, Tempura, Tempura] `shouldBe` GroupResult [TempuraGroup] [Tempura]

    describe "for dumplings" $ do
      it "does not make a group for 0 dumplings" $ do
        group [] `shouldBe` mempty
      it "makes a group from one dumpling" $ do
        group [Dumpling] `shouldBe` GroupResult [DumplingGroup 1] []
      it "makes larger dumpling groups" $ do
        group [Dumpling, Dumpling, Dumpling, Dumpling, Dumpling] `shouldBe` GroupResult [DumplingGroup 5] []
      {-it "makes even groups of 2 tempura" $ do                                           -}
      {-  group [Tempura, Tempura] `shouldBe` GroupResult [TempuraGroup] []                -}
      {-it "adds extras to remainder" $ do                                                 -}
      {-  group [Tempura, Tempura, Tempura] `shouldBe` GroupResult [TempuraGroup] [Tempura]-}
