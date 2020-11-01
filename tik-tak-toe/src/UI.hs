module UI
  ( stateToPicture
  , screenWeight
  , screenHeight
  , cellWeight
  , cellHeight
  ) where

import Graphics.Gloss
import Field

screenWeight :: Int
screenWeight = 300

screenHeight :: Int 
screenHeight = 300

cellWeight :: Int -> Float
cellWeight n = fromIntegral $ div screenWeight n

cellHeight :: Int -> Float
cellHeight n = fromIntegral $ div screenHeight n

boardGridLines :: Int -> Int -> [Picture]
boardGridLines _ (-1) = []
boardGridLines n x = 
  let fx = fromIntegral x in 
  let line1 = line [ (fx * cellWeight n, 0.0)
                   , (fx * cellWeight n, fromIntegral screenHeight)
                   ] in 
  let line2 = line [ (0.0, fx * cellHeight n)
                   , (fromIntegral screenWeight, fx * cellHeight n)
                   ] in 
    line1 : line2 : boardGridLines n (x - 1)

boardGridPicture :: Int -> Picture
boardGridPicture n = pictures $ boardGridLines n n


cellCoords :: Int -> Int -> (Float, Float)
cellCoords n ind = 
  let row    = div ind n in
  let column = mod ind n in
        ( fromIntegral column * (cellWeight n) + (cellWeight n) * 0.5
        , fromIntegral row * (cellHeight n) + (cellHeight n) * 0.5
        )  

cellPicture :: Int -> (Float, Float) -> Cell -> Picture
cellPicture n (x, y) cell = translate x y cellP
  where 
    cellP = case cell of 
              Full XPlayer -> xPicture $ calcPicSize n
              Full OPlayer -> oPicture $ calcPicSize n
              Empty        -> Blank
    calcPicSize n = min (cellWeight n) (cellHeight n)

cellsPictures :: Int -> Int -> [Cell] -> [Picture]
cellsPictures _ _ [] = []
cellsPictures n ind ( x : xs ) =  cellPicture n (cellCoords n ind) x
                                            : cellsPictures n (ind + 1) xs

cellsPicture :: Board -> Picture
cellsPicture board = pictures $ cellsPictures (boardSize board) 0 (boardCells board)

boardPicture :: Board -> Picture
boardPicture board = pictures $ [ boardGridPicture (boardSize board), cellsPicture board, cellsPicture board]

gameOverPicture :: Maybe Player -> Board -> Picture 
gameOverPicture win board = color chooseOverColor (boardPicture board)
  where
    chooseOverColor = case win of
      Just OPlayer -> green
      Just XPlayer -> blue
      Nothing      -> greyN 0.5

runningPicture :: Board -> Picture 
runningPicture board = pictures [boardGridPicture (boardSize board), cellsPicture board, cellsPicture board]

stateToPicture :: Field -> IO Picture
stateToPicture game = return $ translate (fromIntegral screenWeight * (-0.5))
                                         (fromIntegral screenHeight * (-0.5))
                                         frame
  where frame = case gameState game of 
                  Running -> runningPicture board
                  GameOver x -> gameOverPicture x board
        board = gameBoard game

xPicture :: Float -> Picture
xPicture size = pictures [rotate 45.0 line, rotate (135.0) line]
  where
    line = rectangleSolid side 5.0
    side = size * 0.5

oPicture :: Float -> Picture
oPicture size = thickCircle radius 5.0
  where
    radius = size * 0.2