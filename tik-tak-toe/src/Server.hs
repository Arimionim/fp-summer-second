{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( botMove
  , getWinner
  , checkCell
  , startServer
  ) where

import Field
import Data.Foldable
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Servant
import System.Random


type API = "game" :> ReqBody '[JSON] Field :> Get '[JSON] Field
         :<|> "start" :> ReqBody '[JSON] Int :> Get '[JSON] Field

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startServer :: IO ()
startServer = run 8080 app

randMode :: IO GType
randMode = do
  a <- randomIO :: IO Bool
  case a of
    False -> return $ Comp XPlayer
    True -> return $ Comp OPlayer

makeMove :: Field -> Handler Field
makeMove game = do
  return $ botMove game

proceedMove :: Field -> Field
proceedMove game = case gamePlayer game of
  XPlayer -> game {gamePlayer = OPlayer}
  OPlayer -> game {gamePlayer = XPlayer}

initializeGame :: Int -> Handler Field
initializeGame n = do
  liftIO $ putStrLn "player entered"
  rand <- liftIO $ randMode
  return $ botMove $ initialState n rand

server :: Server API
server = makeMove :<|> initializeGame

setBoardCells :: Field -> [Cell] -> Field
setBoardCells game newCell = game {gameBoard = (gameBoard game) {boardCells = newCell}}

checkIfGameOver :: Field -> Field
checkIfGameOver game =
  case getWinner $ gameBoard game of
    Nothing -> if countCells Empty (boardCells $ gameBoard game) == 0
               then game {gameState = GameOver Nothing}
               else game
    Just player -> game {gameState = GameOver $ Just player}

insertToCells :: [Cell] -> Int -> Cell -> [Cell]
insertToCells cells ind cell = 
  let (st, en) = splitAt ind cells in 
  let (_, en') = (head en, tail en) in 
    st <> [cell] <> en'

check :: [Cell] -> Maybe Player
check (cell@(Full player):cells) =
  if all (== cell) cells
  then Just player 
  else Nothing
check _ = Nothing

checkCoords :: Int -> (Int, Int) -> Bool
checkCoords n (x, y) = x >= 0 && y >= 0 && x < n && y < n

getWinner :: Board -> Maybe Player
getWinner board = helper (0, 0)
  where
    n = boardSize board
    cells = boardCells board
    helper :: (Int, Int) -> Maybe Player
    helper (x, y) =
                    if x == n
                    then helper (0, y + 1)
                    else
                      if y == n
                      then Nothing
                      else
                        if checkWinner (x - 1, y - 1) (x, y) (x + 1, y + 1)
                            || checkWinner (x - 1, y) (x, y) (x + 1, y)
                            || checkWinner (x - 1, y + 1) (x, y) (x + 1, y - 1)
                            || checkWinner (x, y - 1) (x, y) (x, y + 1)
                        then
                            case (cells !! (x * n + y)) of
                                Empty -> Nothing
                                Full player -> Just player

                        else helper (x + 1, y)
    checkWinner :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
    checkWinner (x1, y1) (x2, y2) (x3, y3) =
        if not (checkCoords n (x1, y1))
            || not (checkCoords n (x2, y2))
            || not (checkCoords n (x3, y3))
        then
          False
        else
          (cells !! (x1 * n + y1)) == (cells !! (x2 * n + y2))
          && (cells !! (x2 * n + y2)) == (cells !! (x3 * n + y3))
          && (cells !! (x2 * n + y2)) /= Empty

countCells :: Cell -> [Cell] -> Int
countCells cell = length . filter ((==) cell)

checkCell :: [Cell] -> (Int, Int) -> Int -> Bool
checkCell cells (x, y) n = (cells !! (x * n + y) == Empty)

findCell :: Board -> (Int, Int) -> Maybe Int
findCell board (x, y) =
  if x == boardSize board
  then findCell board (0, y + 1)
  else 
    if y == boardSize board
    then Nothing
    else 
      let cells = boardCells board
          n     = boardSize  board in
        if checkCell cells (x, y) n
        then Just $ x * boardSize board + y
        else findCell board (x + 1, y)

botMove :: Field -> Field
botMove game =
  case gameState game of 
    GameOver _ -> game 
    Running -> case gameType game of 
      Humans -> game
      Comp a -> if a == gamePlayer game 
                then makeBotMove game
                else game

makeBotMove :: Field -> Field
makeBotMove game =
    case findCell (gameBoard game) (0, 0) of
      Nothing -> game
      Just ind ->
        let botPlayer = gamePlayer game
            cells = boardCells $ gameBoard game
            newCells = insertToCells cells ind (Full botPlayer) in
          checkIfGameOver $ proceedMove (setBoardCells game newCells)
