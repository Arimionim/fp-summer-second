{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Field
  ( initialState
  , Player (..)
  , Cell (..)
  , Board (..)
  , GType (..)
  , State (..)
  , Field (..)
  ) where

import GHC.Generics ( Generic )
import Data.Aeson

data Cell = Empty | Full Player deriving (Generic, Show, Eq)

data Board = Board
  { boardSize :: Int
  , boardCells :: [Cell]
  } deriving (Generic, Show, Eq)

instance FromJSON Board
instance ToJSON Board where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Cell
instance ToJSON Cell where
  toEncoding = genericToEncoding defaultOptions

data Player = XPlayer | OPlayer deriving (Generic, Show, Eq)

instance FromJSON Player
instance ToJSON Player where 
  toEncoding = genericToEncoding defaultOptions

data GType = Humans | Comp Player deriving (Generic, Show, Eq)

instance FromJSON GType
instance ToJSON GType where
  toEncoding = genericToEncoding defaultOptions

data State = Running | GameOver (Maybe Player) deriving (Generic, Show, Eq)

instance FromJSON State
instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions

data Field = Field 
  { gameBoard :: Board
  , gamePlayer :: Player
  , gameType :: GType
  , gameState :: State
  } deriving (Generic, Eq, Show)

instance FromJSON Field
instance ToJSON Field where
  toEncoding = genericToEncoding defaultOptions

initialState :: Int -> GType -> Field
initialState n gtype = Field { gameBoard = Board n $ replicate (n*n) Empty
                                 , gamePlayer = XPlayer
                                 , gameType = gtype
                                 , gameState = Running }