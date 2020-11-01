module Main where

import Client

main :: IO ()
main = do
  putStrLn "insert field size"
  n <- readLn :: IO Int
  initClient n