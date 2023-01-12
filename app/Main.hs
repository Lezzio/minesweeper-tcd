{-# Language ScopedTypeVariables #-}
module Main (main) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny
import Reactive.Threepenny
import GameBoard
import System.Random ( newStdGen, StdGen )

main :: IO ()
main = do
    gen <- newStdGen
    (boardEvent :: Event (Board -> Board), boardHandler) <- newEvent
    startGUI defaultConfig { jsStatic = Just "./static" } (\window -> setup window boardEvent boardHandler gen)

setup :: Window -> Event (Board -> Board) -> Handler (Board -> Board) -> StdGen -> UI ()
setup window boardEvent boardHandler gen = do
 UI.addStyleSheet window "semantic.css"
 mode <- UI.button # set UI.text "Mode: "
 wrapper <- UI.div # set UI.id_ "minesweeper-wrapper"
 getBody window #+ [element wrapper, element mode]
 -- Grid rendering
 (boardBehavior :: Behavior Board) <- accumB emptyBoard boardEvent
 iterate2D 9 9 (\(x, y) -> cellUI wrapper window boardHandler boardBehavior (x, y) gen)
 return ()

iterate2D :: Int -> Int -> ((Int, Int) -> UI ()) -> UI ()
iterate2D rows cols f = mapM_ f [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]

cellUI :: Element -> Window -> Handler (Board -> Board) -> Behavior Board -> Location -> StdGen -> UI ()
cellUI container window boardHandler boardBehavior (x, y) gen = do
    -- Create a cell as a HTML div and add the default css class to the cell
    cell <- UI.div
    element cell #. "cell hidden"
    -- Sink (bind) the class and text of the cell to the board's current value represented by a behavior
    element cell # sink UI.class_ ((cellClass_ (x, y)) <$> boardBehavior)
    element cell # sink UI.text ((cellTextIndicator (x, y)) <$> boardBehavior)
    getBody window #+ [element cell]
    element container #+ [element cell]
    on UI.click cell $ \_ -> do
        liftIO $ do
            putStrLn $ "Playing" ++ show (x, y)
            boardHandler (\board -> playMove board (x, y) gen)

cellClass_ :: Location -> Board -> String
cellClass_ loc board = convertCellTypeToClassName $ getCellType board loc

convertCellTypeToClassName :: CellType -> String
convertCellTypeToClassName cellType = case cellType of
 Hidden -> "cell hidden"
 Empty -> "cell shown"
 Number n -> "cell shown"
 Bomb -> "cell bomb"

cellTextIndicator :: Location -> Board -> String
cellTextIndicator loc board
    | isDiscovered board loc && isBomb board loc = "💣"
    | isDiscovered board loc = 
      if (countBombNeighbors board loc > 0)
      then show $ countBombNeighbors board loc
      else ""
    | otherwise = ""