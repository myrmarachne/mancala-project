module Main where

import Mancala

main :: IO ()
--main = testFunc
main =
  do
    print(currentPlayer board)
    print(switchPlayer (currentPlayer board) initPlayerList)
    print(currentPlayer board)
    print(getBoardSideForPlayer board (currentPlayer board))
    print(getPointsForPlayer board (currentPlayer board))
    print(makeMove board 3)
    print(makeMove board 2)
    print(checkIfMovePossible board 5)
    where board = initMancalaBoard
