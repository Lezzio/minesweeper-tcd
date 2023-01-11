module Board (board) where

import Data.Set
import qualified Data.Set as Set

type Location = (Int, Int)

data Settings = Settings 
    {
      width :: Int
    , height :: Int
    , bombsCount :: Int 
    }
settings = Settings { width = 9, height = 9, bombsCount = 2 }

data Board = Board
    {
      discoveredCells :: Set Location
    , bombCells :: Set Location
    }

generateBoard :: Board
generateBoard = Board { discoveredCells = empty :: Set Location, bombCells = Data.Set.fromList [(0, 0), (8, 8)] }

isInBoard :: Location -> Bool
isInBoard (x, y) = x >= 0 && x < width settings && y >= 0 && y < height settings

isBomb :: Location -> Board -> Bool
isBomb loc board = member loc (bombCells board)

isDiscovered :: Location -> Board -> Bool
isDiscovered loc board = member loc (discoveredCells board)

locationNeighbors :: Location -> [Location]
locationNeighbors (x, y) = Prelude.filter (/= (x, y)) [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]

locationNeighborsInBoard :: Location -> [Location]
locationNeighborsInBoard (x, y) = Prelude.filter (isInBoard) (locationNeighbors (x,y))