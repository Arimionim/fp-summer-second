module Main where

import Client

main :: IO ()
main = do
  putStrLn "field size: "
  n <- readLn :: IO Int
  initClient n