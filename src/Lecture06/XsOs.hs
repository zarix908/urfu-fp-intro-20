module Lecture06.XsOs where

import Lecture06

printField :: Field -> IO ()
printField field = do
  putStrLn "  1 2 3"
  putStrLn "  _____"
  printLine "1" (field First)
  printLine "2" (field Second)
  printLine "3" (field Third)
  where
    printLine i line = do
      putStr $ i ++ "|"
      putStr $ printCell $ line First
      putStr $ printCell $ line Second
      putStr $ printCell $ line Third
      putStrLn ""
    printCell v = (show v) ++ " "

startXsOs :: IO ()
startXsOs = do
  putStrLn $ "New Game. Xs go first."
  printField emptyField
  (i, j) <- askIndex Cross
  makeStep emptyField i j Cross

nextValue :: Value -> Value
nextValue Cross = Zero
nextValue Zero = Cross
nextValue Empty = Empty

makeStep :: Field -> Index -> Index -> Value -> IO ()
makeStep field i j value = case setCell field i j value of
  Left error -> do
    putStrLn error
    (i, j) <- askIndex value
    makeStep field i j value
  Right newField -> do
    printField newField
    case getGameState newField of
      InProgress -> do
        (i, j) <- askIndex next
        makeStep newField i j next
        where
          next = nextValue value
      result -> do
        putStrLn $ show result

askIndex :: Value -> IO (Index, Index)
askIndex v = do
  putStr $ "set " ++ (show v) ++ " on i j>"
  inputLine <- getLine
  case inputLine of
    (i:_:j:_) -> if isIndexOk i && isIndexOk j
      then return (toIndex i, toIndex j)
      else do
        putStrLn "Incorrect indexes"
        askIndex v
    _ -> do
      putStrLn "Incorrect input line. Don't forget space between numbers"
      askIndex v

toIndex :: Char -> Index
toIndex '1' = First
toIndex '2' = Second
toIndex '3' = Third
toIndex _ = error "wrong index"

isIndexOk :: Char -> Bool
isIndexOk '1' = True
isIndexOk '2' = True
isIndexOk '3' = True
isIndexOk _ = False
