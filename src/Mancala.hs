-- Na razie udostepniane sa wszystkie funkcje - Na koniec dopracowac
module Mancala
    ( MancalaBoard (MkMancalaBoard),
      Player,
      initMancalaBoard,
      getCurrentPlayer,
      switchPlayer,
      initPlayerList,
      getBoardSideForPlayer,
      getPointsForPlayer
    ) where

import Data.List
import Data.Maybe


{- |
  The MancalaBoard data type represents the Game Board. It is actually a list of BoardSides,
  that are representing the amount of stones for each player.
  CurrentPlayer represents the current player.
-}
data MancalaBoard = MkMancalaBoard CurrentPlayer [BoardSide]
  deriving Show -- TODO: Zrobic instance wyswietlajace

{- |
  The BoardSide datatype represents one side of the MancalaBoard for one Player ->
  Board is just a list consisting of integers which are representing the amount of
  stones in every pit and House is just an integer which is equal to the number of
  stones in the House.
-}
data BoardSide = MkBoardSide
  { boardSidePlayer :: Player
  , boardSideBoard  :: Board
  , boardSideHouse  :: House
  }
  deriving Show -- TODO.

type CurrentPlayer = Player
type Board = [Int]
type House = Int


{-  |
  The Player data type is an enumeration type with two data constructors for each
  of the two players (PlayerA and PlayerB).
-}
data Player = PlayerA
            | PlayerB
  deriving (Eq, Show)

-- |Function that initializes players list for mancala.
initPlayerList :: [Player]
initPlayerList = [PlayerA, PlayerB]

-- |Function that returns the number of players.
numberOfPlayers :: [Player] -> Int
numberOfPlayers players = length players

-- |Function that return the index of a Player from a Players list.
getPlayerIndex :: Player -> [Player] -> Int
getPlayerIndex player playersList = fromJust $ elemIndex player playersList

-- |Function that returns the next player from the queue.
switchPlayer :: Player -> [Player] -> Player
switchPlayer player playersList = (!!) initPlayerList $
  (getPlayerIndex player playersList + 1) `mod` numberOfPlayers playersList

-- |Function that returns BoardSide for a player
getBoardSideForPlayer :: MancalaBoard -> Player -> BoardSide
getBoardSideForPlayer (MkMancalaBoard _ boardSides) player =
  findBoardSide boardSides player

findBoardSide :: [BoardSide] -> Player -> BoardSide
findBoardSide boardSides player =
  case boardSides of
    ((MkBoardSide playerX board house):boardSidesTile) ->
      if player == playerX
        then (MkBoardSide player board house)
      else
        findBoardSide boardSidesTile player
    otherwise -> error "No BoardSide connected with provided player in this list"

-- |Function that checks if all of pits on his side are empty.
checkIfPlayerEnded :: BoardSide -> Boolean
checkIfPlayerEnded boardSide = all (== 0) (boardSideBoard boardSide)


--------------- CONSTANTS ---------------
initialNumberOfStones = 4
numberOfPits = 6

-- Pola w listach ponumerowane przeciwnie do ruchu wskazowek zegara!

-- |Overriding standard show to present the Mancala Board in some better way.
-- |TODO: Zastanowic sie nad ladnym wyswietlaniem
--instance Show MancalaBoard where
  --show (MkMancalaBoard stones) =
    --show(stones)

-- |Initial placing stones on Board.
-- Na razie brak mozliwosci wyboru gracza. Na razie gre zaczyna zawsze gracz A. (TODO)
initMancalaBoard :: MancalaBoard
initMancalaBoard = MkMancalaBoard PlayerA
  (map (\player -> (MkBoardSide player (replicate numberOfPits initialNumberOfStones) 0)) initPlayerList)

-- |Function that returns the current player.
getCurrentPlayer :: MancalaBoard -> Player
getCurrentPlayer (MkMancalaBoard currentPlayer _) = currentPlayer

{- |
  Function that returns current score for the player.
  It is the amount of stones in the house, which belongs to that person, plus
  amount of stones in every pit on the side of their board.
-}
getPointsForPlayer :: MancalaBoard -> Player -> Int
getPointsForPlayer mancalaBoard player =
  sum (boardSideBoard boardSide) + (boardSideHouse boardSide)
  where boardSide = getBoardSideForPlayer mancalaBoard player

  {- |
    Function which takes a MancalaBoard and an integer representing
    the number of pit, from which stones are picked.
  -}
{-
makeMove :: MancalaBoard -> Int -> MancalaBoard
makeMove mancalaBoard pitNumber =
  MancalaBoard
    (nextPlayer initPlayerList)
    [(MkBoardSide PlayerA newBoardSideA), (MkBoardSide PlayerB newBoardSideB)]
  where
    currentPlayer = getCurrentPlayer mancalaBoard
    nextPlayer = switchPlayer currentPlayer
-}
