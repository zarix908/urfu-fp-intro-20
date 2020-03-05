module Lecture02Spec where

import Test.Hspec

import Prelude hiding
  (take, length, reverse, sum, concat, map, filter, foldr, foldl)
import qualified Prelude as P
import Lecture02

spec :: Spec
spec = do
  describe "headOr" $ do
    it "headOr 5 (1 :. 2 :. 3 :. Nil) ~> 1" $
      headOr 5 (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` 1
    it "headOr 2 Nil ~> 2" $
      headOr 2 (Nil :: List Int) `shouldBe` 2
  describe "take" $ do
    it "take 2 (1 :. 2 :. 3 :. Nil) ~> (1 :. 2 :. Nil)" $
      take 2 (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` (1 :. 2 :. Nil)
    it "take 50 (1 :. 2 :. 3 :. Nil) ~> (1 :. 2 :. 3 :. Nil)" $
      take 50 (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` (1 :. 2 :. 3 :. Nil)
    it "take 0 (1 :. 2 :. 3 :. Nil) ~> Nil" $
      take 0 (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` Nil
  describe "length" $ do
    it "length Nil ~> 0" $
      length (Nil :: List Int) `shouldBe` 0
    it "length (1 :. Nil) ~> 1" $
      length (1 :. Nil :: List Int) `shouldBe` 1
    it "length (1 :. 2 :. 3 :. Nil) ~> 3" $
      length (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` 3
    it "length ('a' :. 'b' :. Nil) ~> 2" $
      length ('a' :. 'b' :. Nil :: List Char) `shouldBe` 2
  describe "sum" $ do
    it "sum Nil ~> 0" $
      sum (Nil :: List Integer) `shouldBe` 0
    it "sum (1 :. Nil) ~> 1" $
      sum (1 :. Nil :: List Integer) `shouldBe` 1
    it "sum (1 :. 2 :. 3 :. Nil) ~> 3" $
      sum (1 :. 2 :. 3 :. Nil :: List Integer) `shouldBe` 6
    it "sum (104 :. 123 :. 35 :. Nil) ~> 262" $
      sum (104 :. 123 :. 35 :. Nil :: List Integer) `shouldBe` 262
  describe "foldr" $ do
    it "foldr (\\x a -> a + 1) 0 (1 :. 2 :. 3 :. 4 :. Nil) ~> 4" $
      foldr (\_ a -> a + 1) (0 :: Int) (1 :. 2 :. 3 :. 4 :. Nil :: List Int) `shouldBe` 4
    it "foldr (\\x a -> a + x) 0 (1 :. 2 :. 3 :. 4 :. Nil) ~> 10" $
      foldr (\x a -> a + x) (0 :: Int) (1 :. 2 :. 3 :. 4 :. Nil :: List Int) `shouldBe` 10
    it "foldr (-) 0 (1 :. 2 :. 3 :. 4 :. Nil) ~> -2" $
      foldr (-) (0 :: Int) (1 :. 2 :. 3 :. 4 :. Nil :: List Int) `shouldBe` -2
    it "foldr (\\x a -> x) 1234 Nil ~> 1234" $
      foldr (\x _ -> x) (1234::Int) Nil `shouldBe` 1234
    it "foldr (\\x a -> a + 1) 0 ('c' :. 'r' :. Nil) ~> 2" $
      foldr (\_ a -> a + 1) (0::Int) ('c' :. 'r' :. Nil :: List Char) `shouldBe` 2
  describe "map" $ do
    it "map (\\x -> x  + 1) (1 :. 2 :. 3 :. Nil) ~> (2 :. 3 :. 4 :. Nil)" $
      map (\x -> x + 1) (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe`
        (2 :. 3 :. 4 :. Nil)
    it "map id (1 :. 2 :. 3 :. Nil) ~> (2 :. 3 :. 4 :. Nil)" $
      map id (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` (1 :. 2 :. 3 :. Nil)
    it "map (\\x -> x + 1) Nil ~> Nil" $
      map (\x -> x + 1) (Nil :: List Int) `shouldBe` Nil
    it "map (\\x -> \"hi\") (1 :. 2 :. 3 :. Nil) ~> (\"hi\" :. \"hi\" :. \"hi\" :. Nil)" $
      map (\_ -> "hi") (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe`
        ("hi" :. "hi" :. "hi" :. Nil :: List String)
  describe "filter" $ do
    it "filter (\\x -> x  >= 2) (1 :. 2 :. 3 :. Nil) ~> (2 :. 3 :. Nil)" $
      filter (\x -> x >= 2) (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe`
        (2 :. 3 :. Nil)
    it "filter (\\x -> x >= 2) Nil ~> Nil" $
      filter (\x -> x >= 2) (Nil :: List Int) `shouldBe` Nil
    it "filter (\\x -> x == 2) (1 :. 2 :. 3 :. Nil) ~> (2 :. Nil)" $
      filter (\x -> x == 2) (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe`
        (2 :. Nil)
    it "filter (\\x -> length x == 2) (\"hi\" :. \"hello\" :. Nil) ~> (\"hi\" :. Nil)" $
      filter (\x -> P.length x == 2) ("hi" :. "hello" :. Nil :: List [Char]) `shouldBe`
        ("hi" :. Nil)
  describe "foldl" $ do
    it "foldl (\\a x -> a + 1) 0 (1 :. 2 :. 3 :. 4 :. Nil) ~> 4" $
      foldl (\a _ -> a + 1) (0 ::Int) (1 :. 2 :. 3 :. 4 :. Nil :: List Int) `shouldBe` 4
    it "foldl (\\a x -> a + x) 0 (1 :. 2 :. 3 :. 4 :. Nil) ~> 10" $
      foldl (\a x -> a + x) (0 :: Int) (1 :. 2 :. 3 :. 4 :. Nil :: List Int) `shouldBe` 10
    it "foldl (-) 0 (1 :. 2 :. 3 :. 4 :. Nil) ~> -10" $
      foldl (-) 0 (1 :. 2 :. 3 :. 4 :. Nil :: List Int) `shouldBe` -10
    it "foldl (\\a x -> x) 1234 Nil ~> 1234" $
      foldl (\_ x -> x) (1234::Int) Nil `shouldBe` 1234
    it "foldl (\\x a -> a + 1) 0 ('c' :. 'r' :. Nil) ~> 2" $
      foldl (\a _ -> a + 1) (0::Int) ('c' :. 'r' :. Nil :: List Char) `shouldBe` 2
  describe "reverse" $ do
    it "reverse Nil ~> Nil" $
      reverse (Nil :: List Int) `shouldBe` Nil
    it "reverse (1 :. 2 :. 3 :. Nil) ~> (3 :. 2 :. 1 :. Nil)" $
      reverse (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` (3 :. 2 :. 1 :. Nil)
    it "reverse ('a' :. 'b' :. 'c' :. Nil) ~> ('c' :. 'b' :. 'a' :. Nil)" $
      reverse ('a' :. 'b' :. 'c' :. Nil :: List Char) `shouldBe`
        ('c' :. 'b' :. 'a' :. Nil)
  describe "toListH" $ do
    it "toListH (1 :. 2 :. 3 :. Nil) ~> [1, 2, 3]" $
      toListH (1 :. 2 :. 3 :. Nil :: List Int) `shouldBe` [1, 2, 3]
    it "toListH Nil ~> []" $
      toListH (Nil :: List Int) `shouldBe` []
  describe "fromListH" $ do
    it "fromListH [1, 2, 3] ~> (1 :. 2 :. 3 :. Nil)" $
      fromListH [1, 2, 3] `shouldBe` (1 :. 2 :. 3 :. Nil :: List Int)
    it "fromListH [] ~> Nil" $
      fromListH [] `shouldBe` (Nil :: List Int)
  describe "concat" $ do
    it "concat [\"hello \", \"world\"] ~> \"hello, world\"" $
      concat ["hello ", "world"] `shouldBe` "hello world"
    it "concat [[1], [2], [3]] ~> [1, 2, 3]" $
      concat [[1], [2], [3]] `shouldBe` ([1, 2, 3] :: [Int])
  describe "intercalate" $ do
    it "intercalate \", \" [\"hello\" \"world\"] ~> \"hello world\"" $
      intercalate ", " ["hello", "world"] `shouldBe` "hello, world"
    it "intercalate [] [[1], [2], [3]] ~> [1, 2, 3]" $
      intercalate [] [[1], [2], [3]] `shouldBe` ([1, 2, 3] :: [Int])
    it "intercalate [5] [[1], [2], [3]] ~> [1, 5, 2, 5, 3]" $
      intercalate [5] [[1], [2], [3]] `shouldBe` ([1, 5, 2, 5, 3] :: [Int])
