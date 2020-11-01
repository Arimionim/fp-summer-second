module Main
  ( main
  ) where

import Field
import Server
import Test.Hspec (hspec)
import Test.Hspec (SpecWith, describe, shouldBe, it)

drawCells :: [Cell]
drawCells = [   Full XPlayer, Full OPlayer  , Full OPlayer
              , Full OPlayer, Full XPlayer  , Full XPlayer
              , Full XPlayer, Full XPlayer  , Full OPlayer
              ]

drawBoard :: Board
drawBoard = Board 3 drawCells

cellsXWinner :: [Cell]
cellsXWinner = [ Full OPlayer, Full OPlayer, Full XPlayer
         , Empty       , Full XPlayer, Full OPlayer
         , Full XPlayer, Empty       , Empty
         ]

boardXWinner :: Board
boardXWinner = Board 3 cellsXWinner

cellsOWinner :: [Cell]
cellsOWinner = [ Empty       , Full XPlayer, Full OPlayer
         , Full XPlayer, Empty       , Full OPlayer
         , Empty       , Full XPlayer, Full OPlayer
         ]

boardOWinner :: Board
boardOWinner = Board 3 cellsOWinner

checkWinnerTest :: SpecWith ()
checkWinnerTest = describe "checkWinner function test" $ do
  it "check nobody wins" $
    getWinner drawBoard `shouldBe` Nothing
  it "check x wins" $
    getWinner boardXWinner `shouldBe` Just XPlayer
  it "check o wins" $
    getWinner boardOWinner `shouldBe` Just OPlayer

checkCellTest :: SpecWith()
checkCellTest = describe "checkCell test" $ do
  it "check with draw" $ do
    checkCell drawCells (0, 0) 3 `shouldBe` False
    checkCell drawCells (0, 1) 3 `shouldBe` False
    checkCell drawCells (0, 2) 3 `shouldBe` False
    checkCell drawCells (1, 0) 3 `shouldBe` False
    checkCell drawCells (1, 1) 3 `shouldBe` False
    checkCell drawCells (1, 2) 3 `shouldBe` False
    checkCell drawCells (2, 0) 3 `shouldBe` False
    checkCell drawCells (2, 1) 3 `shouldBe` False
    checkCell drawCells (2, 2) 3 `shouldBe` False
  it "check with x winner" $ do
    checkCell cellsXWinner (0, 2) 3 `shouldBe` False
    checkCell cellsXWinner (2, 2) 3 `shouldBe` True
    checkCell cellsXWinner (1, 0) 3 `shouldBe` True
    checkCell cellsXWinner (2, 1) 3 `shouldBe` True
  it "check with o winner" $ do  
    checkCell cellsOWinner (2, 2) 3 `shouldBe` False
    checkCell cellsOWinner (2, 1) 3 `shouldBe` False
    checkCell cellsOWinner (1, 1) 3 `shouldBe` True
    checkCell cellsOWinner (0, 0) 3 `shouldBe` True

main :: IO ()
main = hspec $ do
  checkWinnerTest
  checkCellTest