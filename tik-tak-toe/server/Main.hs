module Main where

import Server

main :: IO ()
main = do
  putStrLn "Hello! I'm tic-tak-toe server. I'm ready to work, bot u need a client to play with me"
  startServer
