module Main where
import Mancala
import AI
import Text.PrettyPrint.Boxes

main :: IO ()
--main = testFunc
main = do
  putStrLn "Do you want to start? (y/n)"
  line <- getLine
  if line == "y"
    then dialog initMancalaBoard PlayerA
    else dialog initMancalaBoard PlayerB

dialog :: MancalaBoard Int -> Player -> IO ()
dialog board player = do
  if gameOver board
    then putStrLn $ show $ getWinner board
    else
      if getCurrentPlayer board == player
        then do
          print board
          putStrLn "Which pit do you want to use? (type 1 - 6)"
          line <- getLine
          let number = read line :: Int
          dialog (makeMove board $ number - 1) player
        else dialog (doAIMove board) player


doAIMove :: MancalaBoard Int -> MancalaBoard Int
doAIMove board = makeMove board (nextMove board 5)
