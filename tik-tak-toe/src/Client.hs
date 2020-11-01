{-# LANGUAGE ParallelListComp #-}

module Client
  ( initClient
  ) where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Field
import UI
import Data.Foldable
import Network.HTTP.Simple

window :: Display
window = InWindow "tic-tac-toe" (screenHeight, screenWeight) (100, 100)

initClient :: Int -> IO ()
initClient n = do
  initState <- initGame n
  playIO window white (30 :: Int) initState stateToPicture changeState (\_ -> return . id)

initGame :: Int -> IO Field
initGame n = do
  request <- parseRequest "GET http://127.0.0.1:8080/start"
--  request <- parseRequest $ "GET http://" ++ host ++ ":" ++ port ++ "/start"
  let jsonRequest = setRequestBodyJSON n request
  response <- httpJSON jsonRequest :: IO (Response Field)
  return $ getResponseBody response

checkCoords :: Int -> (Int, Int) -> Bool
checkCoords n (x, y) = x >= 0 && y >= 0 && x < n && y < n

setBoardCells :: Field -> [Cell] -> Field
setBoardCells game newCell =
    game {gameBoard = (gameBoard game) {boardCells = newCell}}


insertToCells :: [Cell] -> Int -> Cell -> [Cell]
insertToCells cells ind cell =
  let (st, en) = splitAt ind cells in
  let (_, en') = (head en, tail en) in
    st <> [cell] <> en'

applyPlayerMove :: Field -> (Int, Int) -> Field
applyPlayerMove game (x, y) =
    if checkCoords (boardSize $ gameBoard game) (x, y)
    then
      let co = x * (boardSize $ gameBoard game) + y in
      let cells = (boardCells $ gameBoard game) in
        if cells !! co == Empty
        then proceedMove $ setBoardCells game (insertToCells cells co (Full (gamePlayer game)))
        else game
    else game

playerMove :: Field -> (Int, Int) -> Field
playerMove st coords = checkIfGameOver $ applyPlayerMove st coords

proceedMove :: Field -> Field
proceedMove game = case gamePlayer game of
  OPlayer -> game {gamePlayer = XPlayer}
  XPlayer -> game {gamePlayer = OPlayer}

checkIfSomeOneWon :: [Cell] -> Maybe Player
checkIfSomeOneWon (cell@(Full player):cells) =
  if all (== cell) cells
  then Just player
  else Nothing
checkIfSomeOneWon _ = Nothing

getWinner :: Board -> Maybe Player
getWinner board = asum $ map checkIfSomeOneWon $ rows <> cols <> diags1 <> diags2'
  where
    n = boardSize board
    cells = boardCells board
    len = min n 5
    rows  = concat [[[cells !! (i * n + j + q) | q <- [0..len-1]]
                                               | i <- [0..n-1]]
                                               | j <- [0..n-len]]
    cols  = concat [[[cells !! ((i + q) * n + j) | q <- [0..len-1]]
                                                 | i <- [0..n-len]]
                                                 | j <- [0..n-1]]
    diags1 = concat [[[cells !! ((i + q) * n + j + q) | q <- [0..len-1]]
                                                      | i <- [0..n-len]]
                                                      | j <- [0..n-len]]
    diags2 =
      concat [[[if j >= q && n-1-i+q < n then (n - 1 - i + q) * n + j - q else -1
                                                      | q <- [0..len-1]]
                                                      | i <- [0..n-1]]
                                                      | j <- [0..n-1]]
    diags2' = map (\a -> map (cells !!) a) $ filter (\a -> not $ elem (-1 :: Int) a) diags2

countCells :: Cell -> [Cell] -> Int
countCells cell = length . filter ((==) cell)

checkIfGameOver :: Field -> Field
checkIfGameOver game =
  case getWinner $ gameBoard game of
    Nothing -> if countCells Empty (boardCells $ gameBoard game) == 0
               then game {gameState = GameOver Nothing}
               else game
    Just player -> game {gameState = GameOver $ Just player}

mousePosToCellCoord :: Int -> (Float, Float) -> (Int, Int)
mousePosToCellCoord n (x, y) = ( floor ((y + fromIntegral screenHeight * 0.5) / (cellWeight n))
                               , floor ((x + fromIntegral screenWeight * 0.5) / (cellHeight n))
                               )


serverMove :: Field -> IO Field
serverMove game = do
  request' <- parseRequest "GET http://127.0.0.1:8080/game"
  let jsonRequest = setRequestBodyJSON game request'
  response <- httpJSON jsonRequest :: IO (Response Field)
  return $ getResponseBody response

changeState :: Event -> Field -> IO Field
changeState (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  let gameSize = boardSize $ gameBoard game in
    case gameState game of
      Running -> do
        let newSt = playerMove game (mousePosToCellCoord gameSize mousePos)
        newSt' <- serverMove newSt
        return newSt'
      GameOver _ -> serverMove $ initialState gameSize (gameType game)
changeState _ game = return game