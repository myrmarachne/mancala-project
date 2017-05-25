module Main where

import Mancala

main :: IO ()
--main = testFunc
main =
  do
    print(makeMove board 3)
    print(makeMove board 2)
    print(checkIfMovePossible board 5)
    where board = initMancalaBoard
