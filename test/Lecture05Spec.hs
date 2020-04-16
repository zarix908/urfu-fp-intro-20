module Lecture05Spec where

import Test.Hspec
import System.CPUTime

import Lecture05

gen :: Country -> [Country]
gen c@(Country n k) =
  let nextCountry = Country n (toInteger (length n) + k + 1)
  in c : gen nextCountry

epsilon :: Double
epsilon = 0.0000001
areEquivalent :: [Double] -> [Double] -> Bool
areEquivalent xs ys = all (\(x, y) -> abs(x - y) < epsilon) $ zip xs ys

spec :: Spec
spec = do
  describe "nthPrime" $ do
    it "nthPrime 1 ~> 2" $
      nthPrime (3 :: Int) `shouldBe` (5 :: Integer)
    it "nthPrime 3 ~> 5" $
      nthPrime (1 :: Int) `shouldBe` (2 :: Integer)
    it "nthPrime 983 ~> 7753" $
      nthPrime (983 :: Int) `shouldBe` (7753 :: Integer)
  describe "yearGDP" $ do
    it "yearGDP 100 0.1 ~> [100, 100.1, 100.20009(9), 100.3003.., ...]" $
      (take 4 $ yearGDP 100 0.1) `shouldSatisfy` areEquivalent ([100.0,100.1,100.20009999999998,100.30030009999997] :: [Double])
    it "yearGDP 130 10 ~> [130.0,143.0,157.3, ...]" $
      (take 3 $ yearGDP 130 10) `shouldSatisfy` areEquivalent ([130.0,143.0,157.3] :: [Double])
  describe "inHowManyYearsChinaWins" $ do
    it "inHowManyYearsChinaWins ~> 50" $
      inHowManyYearsChinaWins `shouldBe` (50 :: Int)
  describe "stat" $ do
    it "stat [China 80026] ~> [China 80026, Russia 0, Italy 0, USA 0, GreatBritain 0]" $
      stat [Country "China" 80026] `shouldBe` [(Country "China" 80026), (Country "Russia" 0), (Country "Italy" 0), (Country "USA" 0), (Country "GreatBritain" 0)]
    it "stat [China 1000, Italy 47, (Russia 14), (Italy 98), (China 107)] ~> [(China, 1107), (Russia 14), (Italy 145), (USA 0), (GreatBritain 0)]" $
      stat [(Country "China" 1000), (Country "Italy" 47), (Country "Russia" 14), (Country "Italy" 98), (Country "China" 107)] `shouldBe` [(Country "China" 1107), (Country "Russia" 14), (Country "Italy" 145), (Country "USA" 0), (Country "GreatBritain" 0)]
    it "someBigList ~> [(China, 132299370000), (Russia 1263433500), (Italy 2997000), (USA 4497000), (GreatBritain 4062499837500000)]" $ do
      let
        china  = take 210000 $ gen (Country "China" 0)
        russia = take 19000 $ gen (Country "Russia" 0)
        italy  = take 1000 $ gen (Country "Italy" 0)
        usa    = take 1500 $ gen (Country "USA" 0)
        uk     = take 25000000 $ gen (Country "GreatBritain" 0)
      start <- getCPUTime
      isCorrectResults <- pure $! (==)
        (stat (china ++ russia ++ italy ++ usa ++ uk))
        [ (Country "China" 132299370000)
        , (Country "Russia" 1263433500)
        , (Country "Italy" 2997000)
        , (Country "USA" 4497000)
        , (Country "GreatBritain" 4062499837500000)]
      end <- getCPUTime
      let
        okayExecTime = 20
        execTime = (fromIntegral (end - start) `div` 1000000000000) :: Integer
      putStrLn $ "Exec time: " ++ show execTime ++ " seconds\n"
      if execTime < okayExecTime then pure () else do
        putStrLn "!!!!!!!!!!!!\n"
        putStrLn "Function's execution time is too big\n"
        putStrLn $ "Exec time: " ++ show execTime ++ " seconds, should be less than: " ++ show okayExecTime ++ " seconds\n"
        putStrLn "!!!!!!!!!!!!\n"
      if isCorrectResults then pure () else do
        putStrLn "!!!!!!!!!!!!\n"
        putStrLn "Results are incorrect\n"
        putStrLn "!!!!!!!!!!!!\n"
      isCorrectResults `shouldSatisfy` \k -> k && execTime < okayExecTime
