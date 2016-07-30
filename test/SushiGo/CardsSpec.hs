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

  describe "groupSashimi" $ do
    context "with # divisible by 3" $ do
      it "gives a groupResult with the correct groups and no remainder" $ do
        groupSashimi [Sashimi, Sashimi, Sashimi] `shouldBe` HasGroups [SashimiGroup] []


  {-describe "group" $ do                                                                                     -}
  {-  context "for sashimi" $ do                                                                              -}
  {-    it "makes even groups of 3 sashimi" $ do                                                              -}
  {-      group [Sashimi, Sashimi, Sashimi] `shouldBe` GroupResult { groups = [SashimiGroup], remainder = [] }-}
