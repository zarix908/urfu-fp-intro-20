module Lecture06Spec where

import Test.Hspec

import Lecture06

always :: a -> Bool
always _ = True

printRow :: Row -> String
printRow line = "(" ++
  (show $ line First) ++ " " ++ (show $ line Second) ++ " " ++ (show $ line Third) ++
  ")"

printField :: Field -> String
printField field =
  (printRow $ field First) ++ " " ++
  (printRow $ field Second) ++ " " ++
  (printRow $ field Third)

createSetAndPrint :: (Value, Value, Value) -> Index -> Value -> String
createSetAndPrint (x, y, z) i value = printRow $ setCellInRow row i value
  where
    row = createRow x y z

createSetAndPrintField :: Index -> Index -> Value -> String
createSetAndPrintField i j v = case setResult of
  Left message -> message
  Right newField -> printField newField
  where
    setResult = setCell field i j v
    field = createField
      (createRow Empty Zero Cross)
      (createRow Zero Cross Zero)
      (createRow Cross Cross Cross)

fieldFromString :: String -> String -> String -> Field
fieldFromString (x1:x2:x3:[]) (y1:y2:y3:[]) (z1:z2:z3:[]) =
    createField
      (createRow (toValue x1) (toValue x2) (toValue x3))
      (createRow (toValue y1) (toValue y2) (toValue y3))
      (createRow (toValue z1) (toValue z2) (toValue z3))
  where
    toValue :: Char -> Value
    toValue '.' = Empty
    toValue 'o' = Zero
    toValue 'x' = Cross
    toValue x = error (x:"is unknown value ")
fieldFromString _ _ _= error "unknown field sormat"

stateFromString :: String -> String -> String -> GameState
stateFromString x y z = getGameState $ fieldFromString x y z

data MyType = A | B deriving (Eq, Show)

spec :: Spec
spec = do
  describe "f :: [a] -> Int" $ do
    it "f [1,2,3]" $
      f ([1,2,3] :: [Int]) `shouldSatisfy` always
    it "f \"string\"" $
      f "string" `shouldSatisfy` always
    it "f [(1, True), ((-3), False)]" $
      f "string" `shouldSatisfy` always
  describe "g :: (a -> b)->[a]->[b]" $ do
    it "g (\\x -> x) [1,2,3]" $
      g (\x->x) ([1,2,3] :: [Int]) `shouldSatisfy` always
    it "g show \"string\"" $
      g show "string" `shouldSatisfy` always
    it "g fst [(1, True), ((-3), False)]" $
      g fst [(1::Int, True), ((-3), False)] `shouldSatisfy` always
  describe "q :: a -> a -> a" $ do
    it "q 1 1" $
      q 1 (1::Int) `shouldSatisfy` always
    it "q (\\_ -> 1) (\\_ -> 2)" $
      (q (\_ -> 1) (\_ -> 2::Int) (1::Int)) `shouldSatisfy` always
    it "q A::MyType B::MyType" $
      q A B `shouldSatisfy` always
  describe "p :: (a -> b) -> (b -> c) -> (a -> c)" $ do
    it "p (\\x -> x) (\\x -> x)" $
      (p (\x -> x) (\x -> x) 'c') `shouldSatisfy` always
    it "p show length" $
      ((p (show::Int->[Char]) length) 0) `shouldSatisfy` always
  describe "XsOs createField" $ do
    it "createField (. o x) (o x o) (x x x)" $
      let
        field = createField
                  (createRow Empty Zero Cross)
                  (createRow Zero Cross Zero)
                  (createRow Cross Cross Cross)
        line1 = printRow $ field First
        line2 = printRow $ field Second
        line3 = printRow $ field Third
      in line1 ++ line2 ++ line3 `shouldBe` "(. o x)(o x o)(x x x)"
  describe "XsOs setCellInRow" $ do
    it "setCellInRow (. o x) 1 o ~> (o o x)" $
      let
        newRow = createSetAndPrint (Empty, Zero, Cross) First Zero
      in newRow `shouldBe` "(o o x)"
    it "setCellInRow (. o x) 2 x ~> (. x x)" $
      let
        newRow = createSetAndPrint (Empty, Zero, Cross) Second Cross
      in newRow `shouldBe` "(. x x)"
  describe "XsOs setCell" $ do
    it "setCell (. o x) (o x o) (x x x) 1 1 o ~> (o o x) (o x o) (x x x)" $
      let
        newField = createSetAndPrintField First First Zero
      in newField `shouldBe` "(o o x) (o x o) (x x x)"
    it "setCell (. o x) (o x o) (x x x) 2 2 x ~> 'There is 'x' on 2 2'" $
      let
        newField = createSetAndPrintField Second Second Cross
      in newField `shouldBe` "There is 'x' on 2 2"
    it "setCell (. o x) (o x o) (x x x) 2 1 x ~> 'There is 'o' on 2 1'" $
      let
        newField = createSetAndPrintField Second First Zero
      in newField `shouldBe` "There is 'o' on 2 1"
  describe "XsOs getGameState" $ do
    it "getGameState (. x x) (x o x) (o o o) ~> OsWon" $
      (stateFromString ".xx" "xox" "ooo") `shouldBe` OsWon

    it "getGameState (o o x) (o x o) (x x x) ~> XsWon" $
      (stateFromString "oox" "oxo" "xxx") `shouldBe` XsWon

    it "getGameState (x o x) (o o o) (x o x) ~> OsWon" $
      (stateFromString "xox" "ooo" "xox") `shouldBe` OsWon

    it "getGameState (x o x) (o x o) (x o x) ~> XsWon" $
      (stateFromString "xox" "oxo" "xox") `shouldBe` XsWon

    it "getGameState (x o o) (x x o) (x o x) ~> XsWon" $
      (stateFromString "xoo" "xxo" "xox") `shouldBe` XsWon

    it "getGameState (x o x) (x o x) (o x o) ~> Draw" $
      (stateFromString "xox" "xox" "oxo") `shouldBe` Draw

    it "getGameState (. . x) (. o .) (. x .) ~> InProgress" $
      (stateFromString "..x" ".o." ".x.") `shouldBe` InProgress
