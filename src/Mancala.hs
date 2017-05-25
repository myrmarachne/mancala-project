-- Na razie udostepniane sa wszystkie funkcje - Na koniec dopracowac
module Mancala
    ( MancalaBoard (MkMancalaBoard),
      BoardSide (MkBoardSide),
      Player,
      initMancalaBoard,
      initPlayerList,
      makeMove,
      getBoardSidesList,
      getPlayer,
      getBoard,
      initialNumberOfStones,
      numberOfPits
    ) where

--------------- CONSTANTS ---------------
initialNumberOfStones = 4
numberOfPits = 6

--------------- DATA TYPES ---------------
{- |
  The MancalaBoard data type represents the Game Board. It is actually a list of BoardSides,
  that are representing the amount of stones for each player.
  The list is sorted - the first player BoardSide in the BoardSidesList belongs to
  current player.
-}
newtype MancalaBoard a = MkMancalaBoard { getBoardSidesList  :: [BoardSide a] }
  deriving Show -- TODO: Zrobic instance wyswietlajace

{- |
  The BoardSide datatype represents one side of the MancalaBoard for one Player -
  Board is just a list consisting of integers which are representing the amount of
  stones in every pit and the house (the house is the last ).
-}
data BoardSide a = MkBoardSide
  { getPlayer :: Player
  , getBoard  :: Board a
  }
  deriving Show -- TODO.

instance Functor BoardSide where
  fmap f (MkBoardSide player board) = MkBoardSide player (map f board)

instance Applicative BoardSide where
  pure x = MkBoardSide PlayerA (repeat x)
  (MkBoardSide _ board1) <*> (MkBoardSide player board2) =
    MkBoardSide player (zipWith ($) board1 board2)

type Board a = [a]

{-  |
  The Player data type is an enumeration type with two data constructors for each
  of the two players (PlayerA and PlayerB).
-}
data Player = PlayerA
            | PlayerB
  deriving (Eq, Show)

--------------- FUNCTIONS ---------------

-- |Function that initializes players list for mancala.
initPlayerList :: [Player]
initPlayerList = [PlayerA, PlayerB]

-- |Initial placing stones on Board.
-- Na razie brak mozliwosci wyboru gracza. Na razie gre zaczyna zawsze gracz A. (TODO)
initMancalaBoard :: MancalaBoard Int
initMancalaBoard = MkMancalaBoard
  (map (\player -> (MkBoardSide player ((replicate (numberOfPits) initialNumberOfStones) ++ [0] ))) initPlayerList)

fun :: (Num a, Enum a) => (a -> b) -> BoardSide b
fun f = flip fmap (MkBoardSide PlayerA [0..6]) f

move :: Int -> Int -> Int -> (Int -> Int)
move pitNumber stones i
  | i == pitNumber =  const (floor((fromIntegral stones) / 13))
  | i > pitNumber = (+ ceiling((fromIntegral stones - fromIntegral i + fromIntegral pitNumber + 1) / 13))
  | otherwise = (+ floor((fromIntegral stones - fromIntegral i + fromIntegral pitNumber ) / 13))

pick ::  Int -> Int -> Int -> Int -> (Int -> Int)
pick extra pitNumber stones i
  | ((pitNumber > -1) && (i == lastPit)) || ((pitNumber < 0) && (i == oppositeLast))  = const 0
  | (pitNumber > -1) && (i == numberOfPits) = (+ extra)
  | otherwise = move pitNumber stones i
  where
    lastPit = (stones + pitNumber) `mod` 13
    oppositeLast = (11 - stones) `mod` 13

(<**>) :: [BoardSide (Int -> Int)] -> [BoardSide Int] -> [BoardSide Int]
(<**>) = zipWith (<*>)

xD :: (Num a, Enum a) => (a1 -> b -> a -> Int -> Int) -> [(a1, b)] -> [BoardSide Int] -> [BoardSide Int]
xD function parameters boards = (fun <$> (map (uncurry function) $ parameters)) <**> boards

{- |
  Function which takes a MancalaBoard and an integer representing the number of pit, from which stones are picked.
  If number of stones on the chosen pit is equal to (numberOfPits - pitNumber) + 14k for some integer k, than
  the move will end in the house of the current player.
-}

makeMove :: MancalaBoard Int -> Int -> MancalaBoard Int
makeMove mancalaBoard pitNumber =
  if lastPit == 6
  then
    MkMancalaBoard $ xD move (reverse parameters) boards
  else if (lastPit < 6) && (getBoard (boards !! 0) !! lastPit == 0) && stonesNumber < 13
        then MkMancalaBoard $ xD (pick extra) parameters (reverse boards)
       else MkMancalaBoard $ xD move parameters (reverse boards)
  where
    stonesNumber = getBoard (boards !! 0) !! pitNumber
    boards = getBoardSidesList mancalaBoard
    lastPit = (stonesNumber + pitNumber) `mod` 13
    extra = getBoard (boards !! 1) !! (5 - lastPit) + 1
    parameters = [((-1), (stonesNumber + pitNumber - 6)), (pitNumber, stonesNumber)]

---------------- PRZYKLADOWE PLANSZE PO KILKU RUCHACH --------------------------
x = makeMove initMancalaBoard 4
y = makeMove x 1
y' = makeMove y 0
z = makeMove y' 0
