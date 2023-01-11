module GameBoard () where

import Data.Set
import qualified Data.Set as Set

type Location = (Int, Int)

data Settings = Settings
  { width :: Int,
    height :: Int,
    bombsCount :: Int
  }

settings = Settings { width = 9, height = 9, bombsCount = 2 }

data Board = Board
  { discoveredCells :: Set Location,
    bombCells :: Set Location
  }

generateBoard :: Board
generateBoard = Board {discoveredCells = empty :: Set Location, bombCells = Data.Set.fromList [(0, 0), (8, 8), (7, 7)]}

isInBoard :: Location -> Bool
isInBoard (x, y) = x >= 0 && x < width settings && y >= 0 && y < height settings

isBomb :: Board -> Location -> Bool
isBomb board loc = member loc (bombCells board)

isDiscovered :: Board -> Location -> Bool
isDiscovered board loc = member loc (discoveredCells board)

locationNeighbors :: Location -> [Location]
locationNeighbors (x, y) = Prelude.filter (/= (x, y)) [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

locationNeighborsInBoard :: Location -> [Location]
locationNeighborsInBoard (x, y) = Prelude.filter (isInBoard) (locationNeighbors (x, y))

locationNeighborsInBoardNotDiscovered :: Board -> Location -> [Location]
locationNeighborsInBoardNotDiscovered board loc = Prelude.filter (\x -> not $ isDiscovered board x) (locationNeighborsInBoard loc)

hasBombNeighbors :: Board -> Location -> Bool
hasBombNeighbors board loc = foldl1 (||) (Prelude.map (\x -> isBomb board x) (locationNeighborsInBoard loc))

countBombNeighbors :: Board -> Location -> Int
countBombNeighbors board loc = Prelude.foldl (\acc x -> if isBomb board x then acc + 1 else acc) 0 (locationNeighborsInBoard loc)

data CellType = Hidden | Empty | Number Int | Bomb deriving (Eq)

getCellType :: Board -> Location -> CellType
getCellType board loc
  | not $ isDiscovered board loc = Hidden
  | isBomb board loc = Bomb
  | bombCount == 0 = Empty
  | otherwise = Number bombCount
  where bombCount = countBombNeighbors board loc

-- Dummy play move
discoverCell :: Board -> Location -> Board
discoverCell board loc = board { discoveredCells = Data.Set.insert loc (discoveredCells board) }

data PlayResult = Won | Lost | Survived | Invalid

playMove :: Board -> Location -> (Board, PlayResult)
playMove board loc
  | (isDiscovered board loc) = (board, Invalid) -- already discovered, therefore invalid
  | isBomb board loc = (board, Lost)
  | otherwise = (newBoard, if (hasWon newBoard) then Won else Survived)
  where newBoard = propagate board [loc]

propagate :: Board -> [Location] -> Board
propagate board [] = board
propagate board (head:locs) = propagate newBoard newLocs
  where 
    influencedLocs = if (hasBombNeighbors board head) then [] else (locationNeighborsInBoardNotDiscovered board head) 
    newLocs = locs ++ influencedLocs
    newBoard = discoverCell board head

hasWon :: Board -> Bool
hasWon board = False -- TODO implement (remaining cells )

{-
playMove :: Location -> Bool -> GameResult
playMove loc isPlayerMove
    | isBomb loc = Lost
    | isDiscovered loc = Invalid
-}