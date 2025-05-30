module Board2 () where

data Cell = Cell
    { state :: CellState
    , cellType :: CellType
    , mined:: Bool
    , adjacentMinesCount :: Int
    }
data CellState = Discovered | Hidden
data CellType = Empty | Bomb

type Board = [[Cell]]
{- Location inpu, the location to be excluded for a possible bomb location -}
{-generateBoard :: Location -> Board
generateBoard (x, y) = -}

type Location = (Int, Int)
getX :: Location -> Int
getX (x, _) = x
getY :: Location -> Int
getY (_, y) = y

data PlayResult = Survived | Lost

{-
play :: Location -> PlayResult
play (x, y) = 
-}