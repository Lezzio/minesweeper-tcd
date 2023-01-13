module GameBoard (CellType(..), Location, Board(..), PlayResult(..), Settings(..),
 getCellType, playMove, settings, generateBoard, emptyBoard, countBombNeighbors, countFlaggedNeighbors, locationNeighborsInBoardNotDiscoveredAndNotFlagged,
  isDiscovered, isBomb, coordinatesBoard, revealBombs, isFlagged, toggleFlagCell, hasBombNeighbors) where

import Data.Set
import qualified Data.Set as Set
import System.Random
import System.Random.Shuffle (shuffle')
import Debug.Trace
import qualified Data.List as List

type Location = (Int, Int)

-- Data type representing the game settings
data Settings = Settings
  { 
    width :: Int
  , height :: Int
  , bombsCount :: Int
  }

settings :: Settings
settings = Settings { width = 9, height = 9, bombsCount = 10 }

-- Data type containing the board structure
data Board = Board
  { 
    discoveredCells :: Set Location
  , flaggedCells :: Set Location
  , bombCells :: Set Location
  } deriving (Show)

emptyBoard :: Board
emptyBoard = Board {
     discoveredCells = empty :: Set Location
   , flaggedCells = empty :: Set Location
   , bombCells = empty :: Set Location 
   }

-- Checks wether or not a location is within the board boundaries (defined in settings)
isInBoard :: Location -> Bool
isInBoard (x, y) = x >= 0 && x < width settings && y >= 0 && y < height settings

-- Check wether or not a cell at the location provided is a bomb or not
isBomb :: Board -> Location -> Bool
isBomb board loc = member loc (bombCells board)

-- Check wether or not a cell at the location provided is a discovered or not
isDiscovered :: Board -> Location -> Bool
isDiscovered board loc = member loc (discoveredCells board)

-- Discover a cell from the board, return the new board with the discovered cell
discoverCell :: Board -> Location -> Board
discoverCell board loc = board { discoveredCells = Data.Set.insert loc (discoveredCells board) }

-- Check wether or not a cell at the location provided is flagged or not
isFlagged :: Board -> Location -> Bool
isFlagged board loc = member loc (flaggedCells board)

-- Toggle the flag on a cell at the provided location, if it's flagged, unflag it. If it's not flagged, flag it.
toggleFlagCell :: Board -> Location -> Board
toggleFlagCell board loc = if isFlagged board loc 
  then unflagCell board loc -- Already flagged => unflag it
  else flagCell board loc -- Not flagged => flag it

-- Flag the cell at the provided location
flagCell :: Board -> Location -> Board
flagCell board loc = board { flaggedCells = Data.Set.insert loc (flaggedCells board)}

-- Unflag the cell at the provided location
unflagCell :: Board -> Location -> Board
unflagCell board loc = board { flaggedCells = Data.Set.delete loc (flaggedCells board)}

-- Get all the neighbors of a given location
locationNeighbors :: Location -> [Location]
locationNeighbors (x, y) = Prelude.filter (/= (x, y)) [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

-- Get all the neighbors of a given location, but within the boundaries of the board
locationNeighborsInBoard :: Location -> [Location]
locationNeighborsInBoard (x, y) = Prelude.filter isInBoard (locationNeighbors (x, y))

-- Get all the neighbors of a given location, but within the boundaries of the board and that haven't been discovered yet
locationNeighborsInBoardNotDiscovered :: Board -> Location -> [Location]
locationNeighborsInBoardNotDiscovered board loc = Prelude.filter (\x -> not $ isDiscovered board x) (locationNeighborsInBoard loc)

-- USEFUL FOR AI : Get all the neighbors of a given location, but within the boundaries of the board and that haven't been discovered nor flagged yet
locationNeighborsInBoardNotDiscoveredAndNotFlagged :: Board -> Location -> [Location]
locationNeighborsInBoardNotDiscoveredAndNotFlagged board loc = Prelude.filter (\x -> (not $ isDiscovered board x) && (not $ isFlagged board x)) (locationNeighborsInBoard loc)

-- Check wether or not the cell at the provided location has bomb neighbors
hasBombNeighbors :: Board -> Location -> Bool
hasBombNeighbors board loc = foldl1 (||) (Prelude.map (\x -> isBomb board x) (locationNeighborsInBoard loc))

-- Count the number of bombs in the neighbors of the cell at the provided location
countBombNeighbors :: Board -> Location -> Int
countBombNeighbors board loc = Prelude.foldl (\acc x -> if isBomb board x then acc + 1 else acc) 0 (locationNeighborsInBoard loc)

--  USEFUL FOR AI : Count the number of flagged cells in the neighbors of the cell at the provided location
countFlaggedNeighbors :: Board -> Location -> Int
countFlaggedNeighbors board loc = Prelude.foldl (\acc x -> if isFlagged board x then acc + 1 else acc) 0 (locationNeighborsInBoard loc)

-- Data type to caracterise the type of a cell
data CellType = Hidden | Empty | Number Int | Bomb | Flag deriving (Show, Eq)

-- Get the current type of a cell at the given location (it can evolve, going from hidden to empty, or from hidden to bomb for example)
getCellType :: Board -> Location -> CellType
getCellType board loc
  | (isFlagged board loc) && (not $ isDiscovered board loc) = Flag -- Not discovered & flagged => Flag
  | not $ isDiscovered board loc = Hidden -- Not discovered => Hidden
  | isBomb board loc = Bomb -- Discovered & bomb => Bomb
  | bombCount == 0 = Empty -- Discovered & no bomb neighbors => Empty
  | otherwise = Number bombCount -- Discovered & bomb neighbors => Number n (n being the number of bomb neighbors)
  where bombCount = countBombNeighbors board loc

-- Data type to caracterise the result of a played move
data PlayResult = Won | Lost | Survived | Invalid deriving (Eq, Show)

-- Play a move at the given location, returns the new board and a "play result". It asks a StdGen too because the function
-- will generate the board if it's empty. And the generation will exclude the move being played.
playMove :: Board -> Location -> StdGen -> (Board, PlayResult)
playMove board loc gen
  | isDiscovered board loc = (playingBoard, Invalid) -- already discovered, therefore invalid
  | isBomb board loc = (discoverCell playingBoard loc, Lost)
  | otherwise = (newBoard, if hasWon newBoard then Won else Survived)
  where
    -- We generate the board with the move being played excluded as a bomb if the board hasn't been generated before
    -- We have this generation here to ensure we can't play by mistake without having a proper board
    playingBoard = generateIfEmpty board loc gen
    newBoard = propagate playingBoard [loc] -- the new board is the one with the "propgated" loc

-- To win the game the user must have discovered all the empty cells (discovered cells equals to width * height - bomb count)
-- We must therefore also check that all the discovered cells aren't bombs (because the user can discover one bomb and lose)
hasWon :: Board -> Bool
hasWon board = (Set.size $ discoveredCells board) == ((width settings) * (height settings) - (bombsCount settings))

-- The propagate function. It plays a provided list of moves (usually only one the move played)
-- For each location to play, it adds the influenced cells (the undiscovered neighbors). And it keeps going recursively.
-- A location doesn't expand to its undiscovered neighbors when it has a bomb neighbor, that's the reason it stops.
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
  if (Set.null $ bombCells board) -- The board hasn't been generated if there's no bomb at all
     then trace "Generating board..." (generateBoard gen excludedLocation)
     else board

-- Randomly generate a board (bomb count in settings). The provided location, is the location to exclude from bomb's placements
generateBoard :: StdGen -> Location -> Board
generateBoard gen excludedLocation =
  Board { discoveredCells = empty :: Set Location, flaggedCells = empty :: Set Location, bombCells = generatedBombCells }
  where
    coordinates = coordinatesBoard (width settings) (height settings) -- Get all the boards coordinates
    cooridnatesMinusExclusion = List.delete excludedLocation coordinates -- Exclude the location we don't want
    shuffledCoordinates = shuffle' cooridnatesMinusExclusion (length cooridnatesMinusExclusion) gen -- Randomly shuffle the coordinates
    bombCount = bombsCount settings
    generatedBombCells = fromList (Prelude.take bombCount shuffledCoordinates) -- Take the first n coordinates (they will be random), n being the bomb count

-- All the coordinates of a grid from (0, 0) to (rows-1, cols-1)
coordinatesBoard :: Int -> Int -> [(Int, Int)]
coordinatesBoard rows cols = [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]

-- Given a board, returns a board with all the bombs revaled (useful when the game is ended)
revealBombs :: Board -> Board
revealBombs board = Prelude.foldl discoverCell board (bombCells board)