module Lecture03Spec where

import Test.Hspec

import Prelude hiding (and, or, not)
import Interpreter
import qualified Environment as E

import Lecture03



defaultEnv, verboseEnv, libEnv :: E.Environment
defaultEnv = E.defaultEnv { E.color = False }
libEnv = librariesEnv { E.color = False }
verboseEnv = defaultEnv { E.verbose = True }

spec :: Spec
spec = do
  describe "Booleans" $ do
    let
      execEnv = snd $ executeWithEnv defaultEnv $ unlines
        ["ifelse = \\x.x", "true = \\a.\\b.a", "false = \\a.\\b.b"]
    describe "not" $ do
      it "not true ->> false" $ do
        let
          command = unwords ["(" ++ not ++ ")", "true"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.b \8658 false\n\n"
    describe "not" $ do
      it "not false ->> true" $ do
        let
          command = unwords ["(" ++ not ++ ")", "false"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a \8658 true\n\n"
    describe "and" $ do
      it "and true true ->> true" $ do
        let
          command = unwords ["(" ++ and ++ ")", "true", "true"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a \8658 true\n\n"
    describe "and" $ do
      it "and true false ->> false" $ do
        let
          command = unwords ["(" ++ and ++ ")", "true", "false"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.b \8658 false\n\n"
    describe "and" $ do
      it "and false true ->> false" $ do
        let
          command = unwords ["(" ++ and ++ ")", "false", "true"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.b \8658 false\n\n"
    describe "and" $ do
      it "and false false ->> false" $ do
        let
          command = unwords ["(" ++ and ++ ")", "false", "false"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.b \8658 false\n\n"
    describe "or" $ do
      it "or true true ->> true" $ do
        let
          command = unwords ["(" ++ or ++ ")", "true", "true"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a \8658 true\n\n"
    describe "or" $ do
      it "or true false ->> true" $ do
        let
          command = unwords ["(" ++ or ++ ")", "true", "false"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a \8658 true\n\n"
    describe "or" $ do
      it "or false true ->> true" $ do
        let
          command = unwords ["(" ++ or ++ ")", "false", "true"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a \8658 true\n\n"
    describe "or" $ do
      it "or false false ->> false" $ do
        let
          command = unwords ["(" ++ or ++ ")", "false", "false"]
          res = fst $ executeWithEnv execEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.b \8658 false\n\n"
    describe "fib" $ do
      it "fib 0 ->> 1" $ do
        let
          command = unwords ["(" ++ fib ++ ")", "0"]
          res = fst $ executeWithEnv libEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a b \8658 1\n\n"
    describe "fib" $ do
      it "fib 1 ->> 2" $ do
        let
          command = unwords ["(" ++ fib ++ ")", "1"]
          res = fst $ executeWithEnv libEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a (a b) \8658 2\n\n"
    describe "fib" $ do
      it "fib 5 ->> 13" $ do
        let
          command = unwords ["(" ++ fib ++ ")", "5"]
          res = fst $ executeWithEnv libEnv command
        putStrLn res
        res `shouldBe` "\955a.\955b.a (a (a (a (a (a (a (a (a (a (a (a (a b)))))))))))) \8658 13\n\n"
