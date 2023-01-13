module GameBoard (CellType(..), Location, Board, PlayResult(..),
 getCellType, playMove, settings, generateBoard, emptyBoard, countBombNeighbors,
  isDiscovered, isBomb, coordinatesBoard, revealBombs) where

import Data.Set
import qualified Data.Set as Set
import System.Random
import System.Random.Shuffle (shuffle')
import Debug.Trace
import qualified Data.List as List

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
  } deriving (Show)

emptyBoard :: Board
emptyBoard = Board { discoveredCells = empty :: Set Location, bombCells = empty :: Set Location }

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

data PlayResult = Won | Lost | Survived | Invalid deriving (Eq, Show)

-- Populate the board here if it's empty, we have all the information for that
playMove :: Board -> Location -> StdGen -> (Board, PlayResult)
playMove board loc gen
  | (isDiscovered board loc) = (playingBoard, Invalid) -- already discovered, therefore invalid
  | isBomb board loc = (discoverCell playingBoard loc, Lost)
  | otherwise = (newBoard, if hasWon newBoard then Won else Survived)
  where 
    -- We generate the board with the move being played excluded as a bomb if the board hasn't been generated before
    -- We have this generation here to ensure we can't play by mistake without having a proper board
    playingBoard = generateIfEmpty board loc gen
    newBoard = propagate playingBoard [loc]

{-
-- Populate the board here if it's empty, we have all the information for that
playMove :: Board -> Location -> StdGen -> Board
playMove board loc gen
  | isDiscovered playingBoard loc = playingBoard -- already discovered, therefore invalid
  | isBomb playingBoard loc = discoverCell playingBoard loc
  | otherwise = newBoard
  where 
    -- We generate the board with the move being played excluded as a bomb if the board hasn't been generated before
    -- We have this generation here to ensure we can't play by mistake without having a proper board
    playingBoard = generateIfEmpty board loc gen
    newBoard = propagate playingBoard [loc]
-}


-- To win the game the user must have discovered all the empty cells (discovered cells equals to width * height - bomb count)
-- We must therefore also check that all the discovered cells aren't bombs (because the user can discover one bomb and lose)
hasWon :: Board -> Bool
hasWon board = (Set.size $ discoveredCells board) == ((width settings) * (height settings) - (bombsCount settings))

propagate :: Board -> [Location] -> Board
propagate board [] = board
propagate board (head:locs) = propagate newBoard newLocs
  where 
    influencedLocs = 
       if (hasBombNeighbors board head)
        then [] 
        else (locationNeighborsInBoardNotDiscovered board head) 
    newLocs = locs ++ influencedLocs
    newBoard = discoverCell board head

-- The location provided is the one to exclude from bomb locations so that the user can't lose on first move
generateIfEmpty :: Board -> Location -> StdGen -> Board
generateIfEmpty board excludedLocation gen = 
  if (Set.null $ bombCells board)
     then trace "Generating board..." (generateBoard gen excludedLocation)
     else board

generateBoard :: StdGen -> Location -> Board
generateBoard gen excludedLocation = Board {discoveredCells = empty :: Set Location, bombCells = generatedBombCells }
  where
    coordinates = coordinatesBoard 9 9
    cooridnatesMinusExclusion = List.delete excludedLocation coordinates
    shuffledCoordinates = shuffle' cooridnatesMinusExclusion (length cooridnatesMinusExclusion) gen
    bombCount = bombsCount settings
    generatedBombCells = fromList (Prelude.take bombCount shuffledCoordinates)

coordinatesBoard :: Int -> Int -> [(Int, Int)]
coordinatesBoard rows cols = [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]

revealBombs :: Board -> Board
revealBombs board = Prelude.foldl discoverCell board (bombCells board)

{-
playMove :: Location -> Bool -> GameResult
playMove loc isPlayerMove
    | isBomb loc = Lost
    | isDiscovered loc = Invalid
-}