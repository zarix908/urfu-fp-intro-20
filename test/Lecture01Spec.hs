module Lecture01Spec where

import Test.Hspec
import Test.QuickCheck

import Lecture01

spec :: Spec
spec = do
  describe "tellSign" $ do
    it "если n = 0, то \"zero\"" $
      tellSign 0 `shouldBe` "zero"
    it "если n > 0, то \"positive\"" $
      property $
        \x -> tellSign (getPositive x :: Int) === "positive"
    it "если n < 0, то \"negative\"" $
      property $
        \x -> tellSign (getNegative x :: Int) === "negative"
  describe "howManyDigits" $ do
    it "если n >= 0 и n < 10, то \"single\"" $ property $
      forAll (choose (0, 9)) $ \x -> howManyDigits x === "single"
    it "если n >= 10 и n < 100, то \"two-digits\"" $ property $
      forAll (choose (10, 99)) $ \x -> howManyDigits x === "two-digits"
    it "если n >= 100, то \"three-digits or more\"" $ property $
      forAll (suchThat arbitrary (>100)) $
        \x -> howManyDigits x === "three-digits or more"
  describe "describeNumber" $ do
    it "если n = 0, то \"zero single\"" $
      describeNumber 0 `shouldBe` "zero single"
    it "если n > 0 и n < 10, то \"positive single\"" $ property $
      forAll (choose (1, 9)) $
        \x -> describeNumber x === "positive single"
    it "если n < 0 и n > -10, то \"negative single\"" $ property $
      forAll (choose (-1, -9)) $
        \x -> describeNumber x === "negative single"
    it "если n >= 10 и n < 100, то \"positive two-digits\"" $ property $
      forAll (choose (10, 99)) $
        \x -> describeNumber x === "positive two-digits"
    it "если n <= -10 и n > -100, то \"negative two-digits\"" $ property $
      forAll (choose (-10, -99)) $
        \x -> describeNumber x === "negative two-digits"
    it "если n >= 100, то \"positive three-digits or more\"" $ property $
      forAll (suchThat arbitrary (>100)) $
        \x -> describeNumber x === "positive three-digits or more"
    it "если n <= -100, то \"negative three-digits or more\"" $ property $
      forAll (suchThat arbitrary (<= (-100))) $
        \x -> describeNumber x === "negative three-digits or more"
  describe "factorial" $ do
    it "если n = 0, то \"1\"" $
      factorial 0 `shouldBe` 1
    it "если n = 1, то \"1\"" $
      factorial 1 `shouldBe` 1
    it "если n = 2, то \"2\"" $
      factorial 2 `shouldBe` 2
    it "если n = 3, то \"6\"" $
      factorial 3 `shouldBe` 6
    it "если n = 12, то \"479001600\"" $
      factorial 12 `shouldBe` 479001600
    it "если n = 30, то \"265252859812191058636308480000000\"" $
      factorial 30 `shouldBe` 265252859812191058636308480000000
  describe "digitsCount" $ do
    it "если n = 0, то \"1\"" $
      digitsCount 0 `shouldBe` 1
    it "если n = 2, то \"1\"" $
      digitsCount 2 `shouldBe` 1
    it "если n = 12, то \"2\"" $
      digitsCount 12 `shouldBe` 2
    it "если n = -123, то \"3\"" $
      digitsCount (-123) `shouldBe` 3
    it "если n = 1235656, то \"7\"" $
      digitsCount 1235656 `shouldBe` 7
    it "если n = -1235656000000, то \"13\"" $
      digitsCount (-1235656000000) `shouldBe` 13
