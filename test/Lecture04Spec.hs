module Lecture04Spec where

import Test.Hspec
import Control.Monad

import Lecture04

spec :: Spec
spec = do
  describe "Maybe" $ do
    describe "uncons" $ do
      it "uncons [1] ~> (Just 1, [])" $
        uncons ([1] :: [Int]) `shouldBe` (Just 1, [])
      it "uncons [] ~> (Nothing, [])" $
        uncons ([] :: [Int]) `shouldBe` (Nothing, [])
      it "uncons [1, 2, 3] ~> (Just 1, [2, 3])" $
        uncons ([1, 2, 3] :: [Int]) `shouldBe` (Just 1, [2, 3 ])
    describe "zipMaybe" $ do
      it "zipMaybe Nothing (Just 2) ~> Nothing" $
        zipMaybe Nothing (Just 2) `shouldBe` (Nothing :: Maybe (Int, Int))
      it "zipMaybe (Just \"hello\") Nothing ~> Nothing" $
        zipMaybe (Just "hello") Nothing `shouldBe` (Nothing :: Maybe (String, Int))
      it "zipMaybe (Just \"hey\") (Just 2) ~> Just (\"hey\", 2)" $
        zipMaybe (Just "hey") (Just 2) `shouldBe` (Just ("hey", 2) :: Maybe (String, Int))
  describe "Either" $ do
    describe "adopt" $ do
      it "adopt lion ~> Can't adopt lions :(" $
        adopt (AnimalWithType 2 "Leo" Lion) `shouldBe` Left "Can't adopt lions :("
      it "adopt cat 6 years old ~> Can't adopt cat" $
        adopt (AnimalWithType 6 "Harvey" Cat) `shouldBe` Left "Can't adopt cat"
      it "adopt cat 4 years old called Diego ~> Can't adopt cat" $
        adopt (AnimalWithType 4 "Diego" Cat) `shouldBe` Left "Can't adopt cat"
      it "adopt cat 4 years old called Miles ~> Adopted" $
        let animal = AnimalWithType 4 "Miles" Cat in
        adopt animal `shouldBe` Right (AdoptedAnimal animal)
      it "adopt dog 1 years old ~> Can't adopt dog" $
        adopt (AnimalWithType 1 "Diego" Dog) `shouldBe` Left "Can't adopt dog"
      it "adopt dog 2 years old called Spike ~> Adopted" $
        let animal = AnimalWithType 2 "Spike" Dog in
        adopt animal `shouldBe` Right (AdoptedAnimal animal)
      it "adopt duck called NotDaisy ~> Quack" $
        adopt (AnimalWithType 1 "NotDaisy" Duck) `shouldBe` Left "Quack"
      it "adopt duck called Daisy ~> Adopted" $
        let animal = AnimalWithType 2 "Daisy" Duck in
        adopt animal `shouldBe` Right (AdoptedAnimal animal)
  describe "Tree" $ do    
    describe "getValue" $ do    
      it "getValue empty ~> Nothing" $
        getValue (empty :: Tree Int) `shouldBe` Nothing
    describe "isLeaf" $ do    
      it "isLeaf empty ~> True" $
        isLeaf (empty :: Tree Int) `shouldBe` True
    describe "isNode" $ do    
      it "isNode empty ~> False" $
        isNode (empty :: Tree Int) `shouldBe` False
    describe "insert" $ do
      it "insert 3 empty ~> Node Leaf 3 Leaf" $ do
        getValue (insert (3 :: Int) empty) `shouldBe` Just 3
      it "insert 1 $ insert 3 $ insert 2 empty ~> Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)" $ do
        let
          t = insert 1 $ insert 3 $ insert (2 :: Int) empty
        getValue t `shouldBe` Just 2
        (join $ getValue <$> getLeft t) `shouldBe` Just 1
        (join $ getValue <$> getRight t) `shouldBe` Just 3
      it "insert 3 $ insert 2 $ insert 1 empty ~> Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf))" $ do
        let
          t = insert 3 $ insert 2 $ insert (1 :: Int) empty
        getValue t `shouldBe` Just 1
        (join $ getValue <$> getRight t) `shouldBe` Just 2
        (join $ getValue <$> (join $ getRight <$> getRight t)) `shouldBe` Just 3
      it "insert 1 $ insert 2 $ insert 3 empty ~> Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf" $ do
        let
          t = insert 1 $ insert 2 $ insert (3 :: Int) empty
        getValue t `shouldBe` Just 3
        (join $ getValue <$> getLeft t) `shouldBe` Just 2
        (join $ getValue <$> (join $ getLeft <$> getLeft t)) `shouldBe` Just 1
    describe "isElem" $ do
      it "isElem 2 empty ~> False" $ do
        isElem 2 (empty :: Tree Int) `shouldBe` False
      it "isElem 1 $ insert 1 empty ~> True" $ do
        isElem 1 (insert 1 $ empty :: Tree Int) `shouldBe` True
      it "isElem 1 $ insert 1 $ insert 3 $ insert 2 empty ~> True" $ do
        isElem 1 (insert 1 $ insert 3 $ insert 2 (empty :: Tree Int)) `shouldBe` True
      it "isElem 2 $ insert 1 $ insert 3 $ insert 2 empty ~> True" $ do
        isElem 2 (insert 1 $ insert 3 $ insert 2 (empty :: Tree Int)) `shouldBe` True
      it "isElem 4 $ insert 1 $ insert 3 $ insert 2 empty ~> False" $ do
        isElem 4 (insert 1 $ insert 3 $ insert 2 (empty :: Tree Int)) `shouldBe` False
