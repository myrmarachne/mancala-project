-- |header
module Mancala
    ( testFunc,
      MancalaBoard(MkMancalaBoard)
    ) where

-- |Na potrzeby testowania.
testFunc :: IO ()
testFunc = putStrLn "testFunc"

{- |
  The MancalaBoard type represents the Game Board. It is actually a list of integers,
  that are representing the amount of stones.
-}
newtype MancalaBoard = MkMancalaBoard [Int]

-- |Overriding standard show to present the Mancala Board in some better way.
-- |TODO. Zastanowic sie nad ladnym wyswietlaniem
instance Show MancalaBoard where
  show (MkMancalaBoard stones) =
    show(stones)
