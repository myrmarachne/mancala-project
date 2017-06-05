module AI
    (
    nextMove
    ) where

import Mancala
import Data.List (maximumBy)
import Data.Ord (comparing)

type Move = Int

{- |
  Function that suggests a move with the highest score according to the score
  of the player after a sequence of stepDepth moves, when fot n-th move
  current plater chooses the best move according to stepDepth d_n.
-}
nextMove :: MancalaBoard Int -> Int -> Move
nextMove mancalaBoard stepDepth = lookahead mancalaBoard stepDepth -- stepDepth >= 4

{- |
  Function that chooses a move with the highest score according to the score
  of the player after a sequence of stepDepth moves.
-}
lookahead :: MancalaBoard Int -> Int -> Move
lookahead mancalaBoard stepDepth = fst . (maximumBy (comparing snd)) $
  (map (\x -> (x, getScore (makeMove mancalaBoard x) player stepDepth)) (possibleMoves mancalaBoard))
  where
    player =  getPlayer $ getBoardSidesList mancalaBoard !! 0

{- |
  Function that returns a list of possible moves for current player according to
  the function checkIfMovePossible.
 -}
possibleMoves :: MancalaBoard Int -> [Int]
possibleMoves mancalaBoard = filter (checkIfMovePossible mancalaBoard) [0..5]

getScore :: MancalaBoard Int -> Player -> Int -> Int
getScore mancalaBoard player 0 = getPointsForPlayer mancalaBoard player
getScore mancalaBoard player depth
  | gameOver mancalaBoard = if (getWinner mancalaBoard == player)
                              then initialNumberOfStones * numberOfPits * 2 + 1
                              else (-initialNumberOfStones * numberOfPits * 2 - 1)
  | otherwise = getScore (makeMove mancalaBoard (lookahead mancalaBoard (depth-1))) player (depth - 1)

---------------- PRZYKLADOWE PLANSZE PO KILKU RUCHACH --------------------------
x = makeMove initMancalaBoard 4
y = makeMove x 1
y' = makeMove y 0
z = makeMove y' 0
