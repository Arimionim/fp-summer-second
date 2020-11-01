{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( botMove
  , getWinner
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

checkIfSomeOneWon :: [Cell] -> Maybe Player
checkIfSomeOneWon (cell@(Full player):cells) =
  if all (== cell) cells
  then Just player 
  else Nothing
checkIfSomeOneWon _ = Nothing

getWinner :: Board -> Maybe Player 
getWinner board = asum $ map checkIfSomeOneWon $ rows <> cols <> diags1 <> diags2
  where 
    n = boardSize board
    cells = boardCells board
    len = min n 5
    rows  = concat [[[cells !! (i * n + j + q) | q <- [0 ..len-1]]
                                               | i <- [0..n-1]] 
                                               | j <- [0..n-len]]
    cols  = concat [[[cells !! ((i + q) * n + j) | q <- [0..len-1]]
                                                 | i <- [0..n-len]]
                                                 | j <- [0..n-1]]
    diags1 = concat [[[cells !! ((i + q) * n + j + q) | q <- [0..len-1]]
                                                      | i <- [0..n-len]]
                                                      | j <- [0..n-len]]
    diags2 = map (\a -> map (cells !!) a) $ filter (\a -> not $ elem (-1 :: Int) a) diags2Helper

    diags2Helper = concat [[[if j >= q && n-1-i+q < n then (n - 1 - i + q) * n + j - q else -1
                                                      | q <- [0..len-1]]
                                                      | i <- [0..n-1]]
                                                      | j <- [0..n-1]]

countCells :: Cell -> [Cell] -> Int
countCells cell = length . filter ((==) cell)

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
        if cells !! (x * n + y) == Empty
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
