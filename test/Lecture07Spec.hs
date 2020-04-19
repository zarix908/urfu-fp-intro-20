module Lecture07Spec where

import Test.Hspec

import Lecture07
import Lecture07.Money

spec :: Spec
spec = do
  describe "show Expr" $ do
    it "Number 0 ~> 0" $
      show (Number 0) `shouldBe` "0"
    it "Plus (Number 0) (Number 3) ~> 0 + 3" $
      show (Plus (Number 0) (Number 3)) `shouldBe` "0 + 3"
    it "Abs (Plus (Number 0) (Number 3)) ~> |0 + 3|" $
      show (Abs (Plus (Number 0) (Number 3))) `shouldBe` "|0 + 3|"
    it "UnaryMinus (Plus (Mult (Number 2) (Number 2)) (Number 2)) ~> -((2 * 2) + 2)" $
      show (UnaryMinus (Plus (Mult (Number 2) (Number 2)) (Number 2))) `shouldBe` "-((2 * 2) + 2)"
  describe "Vec" $ do
    it "([1,2,3] <> [4,5,6]) <> [7,8,9] = [12,15,18]" $
      (Vec [4,5,6] <> Vec [1,2,3]) <> Vec [7,8,9] `shouldBe` (Vec [12,15,18] :: Vec Integer)
    it "[1,2,3] <> ([4,5,6] <> [7,8,9]) = [12,15,18]" $
      Vec [1,2,3] <> (Vec [4,5,6] <> Vec [7,8,9]) `shouldBe` (Vec [12,15,18] :: Vec Integer)
  describe "LogEntry" $ do
    it "(\"this\" <> \"is\") <> \"text\" = \"thisistext\"" $
      (LogEntry "this" <> LogEntry "is") <> LogEntry "text" `shouldBe` LogEntry "thisistext"
    it "\"this\" <> (\"is\" <> \"text\") = \"thisistext\"" $
      LogEntry "this" <> (LogEntry "is" <> LogEntry "text") `shouldBe` LogEntry "thisistext"
  describe "Money USD" $ do
    it "($13 <> $50) <> $20 = $83" $
      (mkDollars 13 <> mkDollars 50) <> mkDollars 20 `shouldBe` mkDollars 83
    it "$13 <> ($50 <> $20) = $83" $
      mkDollars 13 <> (mkDollars 50 <> mkDollars 20) `shouldBe` mkDollars 83
  describe "Money RUB" $ do
    it "(13 rub <> 50 rub) <> 20 rub = 83 rub" $
      (mkRubbles 13 <> mkRubbles 50) <> mkRubbles 20 `shouldBe` mkRubbles 83
    it "13 rub <> (50 rub <> 20 rub) = 83 rub" $
      mkRubbles 13 <> (mkRubbles 50 <> mkRubbles 20) `shouldBe` mkRubbles 83
  describe "ExactlyOne" $ do
    it "((+) 1) <$> ExactlyOne 10 = ExactlyOne 11" $
      ((+) 1) <$> ExactlyOne 10 `shouldBe` (ExactlyOne 11 :: ExactlyOne Integer)
  describe "Maybe'" $ do
    it "((+) 1) <$> Nothing' = Nothing'" $
      ((+) 1) <$> Nothing' `shouldBe` (Nothing' :: Maybe' Integer)
    it "((+) 1) <$> Just' 3 = Just' 4" $
      ((+) 1) <$> Just' 3 `shouldBe` (Just' 4 :: Maybe' Integer)
  describe "List'" $ do
    it "((+) 1) <$> Nil = Nil" $
      ((+) 1) <$> Nil `shouldBe` (Nil :: List Integer)
    it "((+) 1) <$> Cons 2 (Cons 3 (Cons 4 Nil)) = Cons 3 (Cons 4 (Cons 5 Nil))" $
      ((+) 1) <$> Cons 2 (Cons 3 (Cons 4 Nil)) `shouldBe` (Cons 3 (Cons 4 (Cons 5 Nil)) :: List Integer)
  describe "FileTree" $ do
    let
      tree :: FileTree FileInfo
      tree = Dir "a"
        [ File "b" (FileInfo 1 "2010-03-04")
        , Empty
        , File "c" (FileInfo 3 "2009-03-01")
        , Dir "d" []
        ]
    it "fileSizeOfTree tree = 4" $
      fileSizeOfTree tree `shouldBe` 4
    it "latestModified tree = 2010-03-04" $
      latestModified tree `shouldBe` "2010-03-04"
