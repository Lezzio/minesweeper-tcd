module IA (findSafePlays) where

import Data.Set
import qualified Data.Set as Set
import qualified Data.List as List
import GameBoard

-- IA

-- Find the safest best move, if not possible send the first cell possible to play, return (Maybe bestMove, Maybe bestFlag)
findSafePlays :: Board -> (Maybe Location, Maybe Location)
findSafePlays board = recursiveFindSafePlays
 board (Prelude.filter (\x -> (isDiscovered board x) && (hasBombNeighbors board x)) (coordinatesBoard (width settings) (height settings))) -- All the cells discovered with bomb neighbors

-- The recursive (Maybe bestMove, Maybe bestFlag)
recursiveFindSafePlays :: Board -> [Location] -> (Maybe Location, Maybe Location)
-- base case (couldn't find a safe move), send the first cell possible to play
recursiveFindSafePlays board [] = (bestGuess, Nothing) 
    where 
        remainingDiscoveries = Prelude.filter (\x -> (not $ isDiscovered board x)) (coordinatesBoard (width settings) (height settings)) -- cells not discovered yet
        bestGuess = if (not $ Prelude.null remainingDiscoveries) -- if remaining discoveries, just play the first one
            then Just $ head remainingDiscoveries
            else Nothing
recursiveFindSafePlays board (c:cs)
    | cellPoints == length cellsOfInterest && (not $ Prelude.null cellsOfInterest) = (Nothing, Just (head cellsOfInterest)) -- if as much points as interests, we flag them
    | cellPoints <= 0 && (not $ Prelude.null cellsOfInterest) = (Just (head cellsOfInterest), Nothing) -- if no cell points and remaining cell of interests, discover them
    | otherwise = recursiveFindSafePlays board cs
    where
        bombCount = countBombNeighbors board c -- the number of bomb in the neighbors
        flaggedNeighborsCount = countFlaggedNeighbors board c -- the number of flagged cells in the neighbors
        cellPoints = bombCount - flaggedNeighborsCount -- cell points is equal to  bomb count minus flagged cells count in the neighbors
        cellsOfInterest = locationNeighborsInBoardNotDiscoveredAndNotFlagged board c -- not discovered cells & not flagged