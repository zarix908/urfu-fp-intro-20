module Lecture00Spec where

import Test.Hspec

import Lecture00

spec :: Spec
spec = do
  describe "everythingWorks" $ do
    it "всё работает и тестовая функция возвращает что ожидалось" $
      everythingWorks `shouldBe` "Welcome!"
