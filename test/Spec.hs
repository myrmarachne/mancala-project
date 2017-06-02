import Test.Framework (defaultMain)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Mancala
import AI

main :: IO ()
main = defaultMain [eqTestSuite, checkIfPlayerEndedTestSuite, getPointsForPlayerTestSuite, gameOverTestSuite, getWinnerTestSuite, checkIfMovePossibleTestSuite, makeMoveTestSuite]

eqTestSuite :: Test
eqTestSuite = testGroup "Testing Eq functions"
   [testProperty "Eq returns true for the same instance of BoardSide" (\(boardSide) -> boardSide == (boardSide :: BoardSide Int)),
   testCase "Eq returns false for BoardSides having difference in pits" (MkBoardSide PlayerA [1, 2, 3, 4, 5, 6, 0] == MkBoardSide PlayerA [2, 2, 3, 4, 5, 6, 0] @?= False),
   testCase "Eq returns false for BoardSides having difference in house" (MkBoardSide PlayerA [1, 2, 3, 4, 5, 6, 0] == MkBoardSide PlayerA [2, 2, 3, 4, 5, 6, 2] @?= False),
   testProperty "Eq returns false for BoardSides with different player" (\(board) -> MkBoardSide PlayerA (board :: [Int]) /= MkBoardSide PlayerB board),
   testProperty "Eq returns true for the same instance of MancalaBoard" (\(mBoard) -> mBoard == (mBoard :: MancalaBoard Int)),
   testProperty "Eq returns false for MancalaBoards when boardsSides order is changed and boardsSides are different"
    (\(boardSide1, boardSide2) -> (MkMancalaBoard [(boardSide1 :: BoardSide Int), (boardSide2 :: BoardSide Int)] == MkMancalaBoard [boardSide2, boardSide1]) == (boardSide1 == boardSide2))]

checkIfPlayerEndedTestSuite :: Test
checkIfPlayerEndedTestSuite = testGroup "Testing checkIfPlayerEnded function"
  [testCase "ends when all pits are zeros" (checkIfPlayerEnded  (MkBoardSide PlayerA [0, 0, 0, 0, 0, 0, 4]) @?= True),
  testCase "does not end when at least one pit is not-zero" (checkIfPlayerEnded (MkBoardSide PlayerA [1, 0, 0, 0, 0, 0, 4]) @?= False),
  testCase "does not end when more than pits are not-zero" (checkIfPlayerEnded (MkBoardSide PlayerA [2, 0, 0, 2, 4, 1, 4]) @?= False)]

gameOverTestSuite :: Test
gameOverTestSuite = testGroup "Testing gameOver function"
  [testProperty "game is over when not current players's all pits have zero stones"
    (\(boardSide) -> gameOver (MkMancalaBoard [(boardSide :: BoardSide Int), MkBoardSide PlayerB [0, 0, 0, 0, 0, 0, 4]])),
  testProperty "game is over when current players's all pits have zero stones"
    (\(boardSide) -> gameOver (MkMancalaBoard [MkBoardSide PlayerA [0, 0, 0, 0, 0, 0, 4], (boardSide :: BoardSide Int)])),
  testCase "game is not over when both players has at least one pit not empty"
    (gameOver (MkMancalaBoard [MkBoardSide PlayerA [3, 5, 1, 2, 4, 2, 12], MkBoardSide PlayerB [0, 1, 0, 0, 0, 0, 4]]) @?= False)]


sampleBoardToGetPointsForPlayer = MkMancalaBoard [MkBoardSide PlayerA [3, 5, 1, 2, 0, 0, 4], MkBoardSide PlayerB [0, 0, 1, 0, 0, 0, 4]]
getPointsForPlayerTestSuite :: Test
getPointsForPlayerTestSuite = testGroup "Testing getPointsForPlayer function"
  [testCase "sum to 15 for current Player" (getPointsForPlayer sampleBoardToGetPointsForPlayer PlayerA @?= 15),
  testCase "sum to 5 for not current Player" (getPointsForPlayer sampleBoardToGetPointsForPlayer PlayerB @?= 5)]

getWinnerTestSuite :: Test
getWinnerTestSuite = testGroup "Testing getWinner function"
  [testCase "wins current player" (getWinner (MkMancalaBoard [MkBoardSide PlayerA [0, 0, 0, 0, 8, 0, 20], MkBoardSide PlayerB [0, 0, 0, 0, 0, 0, 20]]) @?= PlayerA),
  testCase "wins not current player" (getWinner (MkMancalaBoard [MkBoardSide PlayerA [0, 0, 0, 0, 0, 0, 20], MkBoardSide PlayerB [0, 0, 0, 0, 8, 0, 20]]) @?= PlayerB)]

sampleBoardToCheckIfMovePossible = MkMancalaBoard [MkBoardSide PlayerA [0, 0, 0, 0, 8, 3, 20], MkBoardSide PlayerB [1, 3, 4, 6, 2, 5, 20]]
checkIfMovePossibleTestSuite :: Test
checkIfMovePossibleTestSuite = testGroup "Testing checkIfMovePossible function"
  [testProperty "not possible when negative number given" (\(board) -> checkIfMovePossible (board :: MancalaBoard Int) (-5) == False),
  testProperty "not possible when house number given" (\(board) -> checkIfMovePossible (board :: MancalaBoard Int) numberOfPits == False),
  testCase "not possible when empty pit number given" ((checkIfMovePossible sampleBoardToCheckIfMovePossible 2) @?= False),
  testCase "possible when not empty pit number given" ((checkIfMovePossible sampleBoardToCheckIfMovePossible 4) @?= True)]

makeMoveTestSuite :: Test
makeMoveTestSuite = testGroup "Testing makeMove function"
  [testCase "make move inside current player boardSide"
    (makeMove (MkMancalaBoard [MkBoardSide PlayerA [4, 1, 1, 2, 8, 1, 7], MkBoardSide PlayerB [2, 2, 3, 4, 1, 2, 10]]) 2 @?=
       (MkMancalaBoard [MkBoardSide PlayerB [2, 2, 3, 4, 1, 2, 10], MkBoardSide PlayerA [4, 1, 0, 3, 8, 1, 7]])
    ),
  testCase "make move ending in current player's house"
    (makeMove (MkMancalaBoard [MkBoardSide PlayerA [4, 1, 1, 3, 8, 1, 7], MkBoardSide PlayerB [2, 2, 3, 4, 1, 2, 10]]) 3 @?=
       (MkMancalaBoard [MkBoardSide PlayerA [4, 1, 1, 0, 9, 2, 8], MkBoardSide PlayerB [2, 2, 3, 4, 1, 2, 10]])
    ),
  testCase "make move ending in second player's pit"
    (makeMove (MkMancalaBoard [MkBoardSide PlayerB [4, 1, 1, 4, 8, 1, 7], MkBoardSide PlayerA [2, 2, 3, 4, 1, 2, 10]]) 3 @?=
       (MkMancalaBoard [MkBoardSide PlayerA [3, 2, 3, 4, 1, 2, 10], MkBoardSide PlayerB [4, 1, 1, 0, 9, 2, 8]])
    ),
  testCase "make move going through the whole second player's board"
    (makeMove (MkMancalaBoard [MkBoardSide PlayerB [4, 1, 1, 4, 3, 10, 7], MkBoardSide PlayerA [2, 2, 3, 4, 1, 2, 10]]) 5 @?=
       (MkMancalaBoard [MkBoardSide PlayerA [3, 3, 4, 5, 2, 3, 10], MkBoardSide PlayerB [5, 2, 2, 4, 3, 0, 8]])
    ),
  testCase "make move ending on pit with zero stones"
    (makeMove (MkMancalaBoard [MkBoardSide PlayerB [4, 1, 0, 4, 4, 4, 7], MkBoardSide PlayerA [4, 4, 4, 4, 4, 4, 2]]) 1 @?=
       (MkMancalaBoard [MkBoardSide PlayerA [4, 4, 4, 0, 4, 4, 2], MkBoardSide PlayerB [4, 0, 0, 4, 4, 4, 12]])
    )
  ]

---------------------------- Arbitrary instances -------------------------------
instance Arbitrary a => Arbitrary (MancalaBoard a) where
  arbitrary = do
    board1 <- arbitrary
    board2 <- arbitrary
    return $ MkMancalaBoard [board1, board2]

instance Arbitrary a => Arbitrary (BoardSide a) where
  arbitrary = do
    list <- vector $ numberOfPits + 1
    player <- arbitrary
    return $ MkBoardSide player list

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
