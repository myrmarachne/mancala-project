module Main where

import Mancala

main :: IO ()
--main = testFunc
main =
  do
    print(getCurrentPlayer board)
    print(switchPlayer (getCurrentPlayer board) initPlayerList)
    print(getCurrentPlayer board)
    print(getBoardSideForPlayer board (getCurrentPlayer board))
    print(getPointsForPlayer board (getCurrentPlayer board))
    where board = initMancalaBoard
