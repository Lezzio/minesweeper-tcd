{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Control.Monad
import System.Random
import System.Console.CmdArgs

data GameOptions = GameOptions {
    width :: Int
  , height :: Int
  , mineCount :: Int
} deriving (Show, Data, Typeable)

options :: GameOptions
options = GameOptions {
    width = 10
  , height = 10
  , mineCount = 10
}

--data Board = Board [[Bool]] [[Int]] [[Bool]] -- Bombs, Discovered cells (-1 undiscovered, 0 to 8 = bombs), Flags
--            deriving Show

-- Modify board
--playOnce :: Board -> Board

--iaPlayOnce :: Board -> Board

--undiscoverCell :: Board -> Board

data Square = Square { mine :: Bool
                     , neighboringMines :: Int
                     , discovered :: Bool
                     , flag :: Bool
                     } deriving (Show)

type Matrix a = [[a]]
type Board = Matrix Square

data Coord = Coord {
    x :: Int
  , y :: Int 
  } deriving (Eq, Show)

-- https://blog.ramdoot.in/haskell-and-command-line-arguments-ede793736c2
-- using stack

main :: IO ()
main = do
    op <- cmdArgs options
    --putStrLn (show (width op) ++ " " ++ show (height op) ++ " " ++ show (mineCount op) ++ " :)")
    initCoord <- getUserInputCoord
    --putStrLn $ show initCoord
    let list = ([1..((y initCoord)*(width op) + (x initCoord))-1] ++ [((y initCoord)*(width op) + (x initCoord))+1..((width op) * (height op))])
    let list2 = shuffleList list
    let list3 = take (mineCount op) list2
    putStrLn $ show list3
    --list <- randomize ((width op) * (height op)) (mineCount op)
    --putStrLn $ show list
    --putStrLn $ show $ randomize ((width op) * (height op)) (mineCount op)

    --g <- getStdGen
    --putStrLn $ show $ makeBoardList ((width op)*(height op)-((y initCoord)*(width op) + (x initCoord))) ((width op)*(height op)) (mineCount op) [] g

    --play $ makeBoard initCoord 20 20 10

shuffleList :: [Int] -> [Int]
shuffleList x = do
	i <- randomRIO (0, length(x)-1)
	r <- shuffleList (take i x ++ drop (i+1) x)
	return (x!!i : r)


getUserInputCoord :: IO Coord
getUserInputCoord = do
    putStrLn "Choose a tile"
    putStrLn "Column : "
    x <- getLine
    putStrLn "Row : "
    y <- getLine
    return $ Coord (read x - 1) (read y - 1)

--makeBoard :: Coord -> Int -> Int -> Int -> Board
--makeBoard initCoord w h nbMines = Board {}
--makeBoardList (w*h - (y initCoord * w + x initCoord)) w*h mineCount []

--randomize :: Int -> Int -> IO [Int]
--randomize size mineCount = replicateM mineCount (randomRIO (1, size-1))


{-makeBoardList :: Int -> Int -> Int -> [Square] -> StdGen -> [Square]
makeBoardList _ 0 _ boardList _ = boardList -- stop once everything is filled
makeBoardList initPos size remainingMines boardList g
    -- Can't place mine on starting pos
    -- prob of getting a ming
    -- Check if we still have mines to place
    | initPos == size || (randomR (1, size) g) == 1 || remainingMines == 0 = makeBoardList initPos (size-1) remainingMines (boardList ++
        [Square {
            mine = False
        , neighboringMines = 0
        , discovered = False
        , flag = False
        }]) g
    | otherwise = makeBoardList initPos (size-1) (remainingMines-1) (boardList ++ 
        [Square {
            mine = True
        , neighboringMines = 0
        , discovered = False
        , flag = False
        }]) g-}

play :: Board -> Board
play board = board


  -- proba mine : 1 / w * h
  -- quand on retourne en baisse de 1 (minePlaced)
  -- proba 0 si starting coord

  -- transform list to matrix
  -- compute neighbouring mines```