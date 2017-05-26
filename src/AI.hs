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

-- |Function that checks if all of pits on his side are empty.
checkIfPlayerEnded :: (Eq a, Num a) => BoardSide a -> Bool
checkIfPlayerEnded boardSide = all (== 0) (init $ getBoard boardSide)

{- |
  Function that returns current score for the player.
  It is the amount of stones in the house, which belongs to that person, plus
  amount of stones in every pit on the side of their board.
-}
getPointsForPlayer :: MancalaBoard Int -> Player -> Int
getPointsForPlayer mancalaBoard player =
  sum (getBoard (getBoardSideForPlayer mancalaBoard player))

-- |Function that returns BoardSide for a player
getBoardSideForPlayer :: MancalaBoard a -> Player -> BoardSide a
getBoardSideForPlayer mancalaBoard player
  | getPlayer (boards !! 0) == player = boards !! 0
  | otherwise  = boards !! 1
  where boards = getBoardSidesList mancalaBoard

-- |Function that checks if wished move is possible. TODO Sprawdzic czy poprawne
checkIfMovePossible :: MancalaBoard Int -> Int -> Bool
checkIfMovePossible _ pitNumber
  | pitNumber < 0 = False
  | pitNumber >= numberOfPits = False -- house'a tez nie mozna ruszac
checkIfMovePossible mancalaBoard pitNumber
  | stonesNumber < 1 = False
  | otherwise = True
  where
    stonesNumber = getBoard (boards !! 0) !! pitNumber
    boards = getBoardSidesList mancalaBoard
{- |
  Function that returns a list of possible moves for current player according to
  the function checkIfMovePossible.
 -}
possibleMoves :: MancalaBoard Int -> [Int]
possibleMoves mancalaBoard = filter (checkIfMovePossible mancalaBoard) [0..5]

{- |
  Function that checks if one of the player has won the game
  (has all of his pits empty, excluding the house)
 -}
gameOver :: MancalaBoard Int -> Bool
gameOver mancalaBoard = all (== True) $ map (checkIfPlayerEnded) (getBoardSidesList mancalaBoard)

getWinner :: MancalaBoard Int -> [Player]
getWinner mancalaBoard = filter (\x -> numberOfPits * initialNumberOfStones
  <= (getPointsForPlayer mancalaBoard x)) initPlayerList

getScore :: MancalaBoard Int -> Player -> Int -> Int
getScore mancalaBoard player 0 = getPointsForPlayer mancalaBoard player
getScore mancalaBoard player depth
  | gameOver mancalaBoard = if (getWinner mancalaBoard == [player])
                              then initialNumberOfStones * numberOfPits * 2 + 1
                              else (-initialNumberOfStones * numberOfPits * 2 - 1)
  | otherwise = getScore (makeMove mancalaBoard (lookahead mancalaBoard (depth-1))) player (depth - 1)

---------------- PRZYKLADOWE PLANSZE PO KILKU RUCHACH --------------------------
x = makeMove initMancalaBoard 4
y = makeMove x 1
y' = makeMove y 0
z = makeMove y' 0
