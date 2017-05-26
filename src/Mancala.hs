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


---------------------------------- CONSTANTS -----------------------------------
{- |
  It is a constant used for the purposes of generating the game - it is
  the initial number of stones placed in every pit in the beginning.
-}
initialNumberOfStones = 4
{- |
  It is a constant used for the purposes of generating the game - it is
  the number of pits every side of the board consists of.
-}
numberOfPits = 6

---------------------------------- DATA TYPES ----------------------------------
{- |
  The MancalaBoard data type represents the Game Board. It is actually a list of
  BoardSides, that are representing the amount of stones for each player in every
  pit and in the house. The list of BoardSides is sorted - the first BoardSide
  in the BoardSidesList belongs to the current player.
-}
newtype MancalaBoard a = MkMancalaBoard { getBoardSidesList  :: [BoardSide a] }
  deriving Show -- TODO: Zrobic instance wyswietlajace

{- |
  The BoardSide data type represents one side of the mancala Board for one player -
  Board is just a list consisting of numbers which are representing the amount of
  stones in every pit and the house (the house is the last element of the list).
-}
data BoardSide a = MkBoardSide { getPlayer :: Player,
                                 getBoard  :: Board a
                               } deriving Show -- TODO.

instance Functor BoardSide where
  fmap f (MkBoardSide player board) = MkBoardSide player (map f board)

instance Applicative BoardSide where
  pure x = MkBoardSide PlayerA (repeat x)
  (MkBoardSide _ board1) <*> (MkBoardSide player board2) =
    MkBoardSide player (zipWith ($) board1 board2)

{- |
  Board is a list consisting of numbers which are representing the amount of
  stones in every pit and the house (the house is the last element of the list).
-}
type Board a = [a]

{-  |
  The Player data type is an enumeration type with two data constructors for each
  of the two players (PlayerA and PlayerB).
-}
data Player = PlayerA
            | PlayerB
  deriving (Eq, Show)

---------------------------------- FUNCTIONS ----------------------------------
{-  |
  Function that initializes players list for mancala game.
-}
initPlayerList :: [Player]
initPlayerList = [PlayerA, PlayerB]

{-  |
  Function that initializes the board for mancala game. At the beginning every
  player has got an empty house and every other pit of the board contains
  initialNumberOfStones stones.
-}
-- Na razie brak mozliwosci wyboru gracza. Na razie gre zaczyna zawsze gracz A. (TODO)
initMancalaBoard :: MancalaBoard Int
initMancalaBoard = MkMancalaBoard
  (map (\player -> (MkBoardSide player ((replicate (numberOfPits) initialNumberOfStones) ++ [0]))) initPlayerList)

fun :: (Num a, Enum a) => (a -> b) -> BoardSide b
fun f = flip fmap (MkBoardSide PlayerA [0..6]) f

move :: Int -> Int -> Int -> (Int -> Int)
move pitNumber stones i
  | i == pitNumber =  const $ floor(shift / 13)
  | i > pitNumber = (+ ceiling ((shift + 1) / 13))
  | otherwise = (+ floor (shift / 13))
  where
    shift = fromIntegral stones - fromIntegral i + fromIntegral pitNumber

pick ::  Int -> Int -> Int -> Int -> (Int -> Int)
pick extra pitNumber stones i
  | ((pitNumber > -1) && (i == lastPit)) || ((pitNumber < 0) && (i == oppositeLast))  = const 0
  | (pitNumber > -1) && (i == numberOfPits) = (+ extra)
  | otherwise = move pitNumber stones i
  where
    lastPit = (stones + pitNumber) `mod` 13
    oppositeLast = (2 * numberOfPits - stones) `mod` 13

(<**>) :: [BoardSide (Int -> Int)] -> [BoardSide Int] -> [BoardSide Int]
(<**>) = zipWith (<*>)

xD :: (a -> b -> Int -> Int -> Int) -> [(a, b)] -> [BoardSide Int] -> [BoardSide Int]
xD function parameters boards = (fun <$> (map (uncurry function) $ parameters)) <**> boards

{- |
  Function which takes a MancalaBoard and an integer representing the number of pit,
  from which stones are being picked. If number of stones on the chosen pit is equal
  to (numberOfPits - pitNumber) + 13k for some integer k, than the move will end in the
  house of the current player and the next turn will also belong to current player.
  If the last stone lands in an empty pit, which belongs to the current player,
  all of the stones placed in an opposite pit land in current players house.
-}
makeMove :: MancalaBoard Int -> Int -> MancalaBoard Int
makeMove mancalaBoard pitNumber =
  if lastPit == numberOfPits
  then
    MkMancalaBoard $ xD move (reverse parameters) boards
  else
    if (lastPit < numberOfPits) &&
      (getBoard (boards !! 0) !! lastPit == 0) &&
      stonesNumber < (2 * numberOfPits + 1)
    then MkMancalaBoard $ xD (pick extra) parameters (reverse boards)
    else MkMancalaBoard $ xD move parameters (reverse boards)
  where
    stonesNumber = getBoard (boards !! 0) !! pitNumber
    boards = getBoardSidesList mancalaBoard
    lastPit = (stonesNumber + pitNumber) `mod` 13
    extra = getBoard (boards !! 1) !! (numberOfPits - lastPit - 1) + 1
    parameters = [((-1), (stonesNumber + pitNumber - numberOfPits)), (pitNumber, stonesNumber)]

---------------- PRZYKLADOWE PLANSZE PO KILKU RUCHACH --------------------------
x = makeMove initMancalaBoard 4
y = makeMove x 1
y' = makeMove y 0
z = makeMove y' 0
