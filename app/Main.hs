{-# Language ScopedTypeVariables #-}
module Main (main) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny
import Reactive.Threepenny
import GameBoard
import System.Random ( newStdGen )

main :: IO ()
main = do
    (boardEvent :: Event (Board -> Board), boardHandler) <- newEvent
    startGUI defaultConfig { jsStatic = Just "./static" } (\window -> setup window boardEvent boardHandler)

setup :: Window -> Event (Board -> Board) -> Handler (Board -> Board) -> UI ()
setup window boardEvent boardHandler = do
 -- Add the CSS stylesheet
 UI.addStyleSheet window "semantic.css"
 mode <- UI.button # set UI.text "Mode: "
 wrapper <- UI.div # set UI.id_ "minesweeper-wrapper"
 -- Grid rendering
 (boardBehavior :: Behavior Board) <- accumB emptyBoard boardEvent
 iterate2D 9 9 (\(x, y) -> cellUI wrapper window boardHandler boardBehavior (x, y))
 -- Restart button
 restart <- UI.button # set UI.text "Restart game" # set UI.id_ "restart"
 on UI.click restart $ \_ -> do
    body <- getBody window
    -- Trick to reset the game similar to a page refresh, delete the body and call setup again,
    -- finally set the board to empty board
    UI.delete body
    liftIO $ do
        boardHandler (const emptyBoard) 
    setup window boardEvent boardHandler

 getBody window #+ [element wrapper, element mode, element restart]
 return ()

iterate2D :: Int -> Int -> ((Int, Int) -> UI ()) -> UI ()
iterate2D rows cols f = mapM_ f [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]

cellUI :: Element -> Window -> Handler (Board -> Board) -> Behavior Board -> Location -> UI ()
cellUI container window boardHandler boardBehavior (x, y) = do
    -- Create a cell as a HTML div and add the default css class to the cell
    cell <- UI.div
    element cell #. "cell hidden"
    -- Sink (bind) the class and text of the cell to the board's current value represented by a behavior
    element cell # sink UI.class_ ((cellClass (x, y)) <$> boardBehavior)
    element cell # sink UI.text ((cellTextIndicator (x, y)) <$> boardBehavior)
    getBody window #+ [element cell]
    element container #+ [element cell]
    on UI.click cell $ \_ -> do
        liftIO $ do
            putStrLn $ "Playing" ++ show (x, y)
            gen <- newStdGen
            boardHandler (\board -> playMove board (x, y) gen)

cellClass :: Location -> Board -> String
cellClass loc board = convertCellTypeToClassName $ getCellType board loc

convertCellTypeToClassName :: CellType -> String
convertCellTypeToClassName cellType = case cellType of
 Hidden -> "cell hidden"
 Empty -> "cell shown"
 Number n -> "cell shown" ++ " " ++ numberColorClass n
 Bomb -> "cell bomb"

numberColorClass :: Int -> String
numberColorClass number
 | number == 1 = "blue-text"
 | number == 2 = "green-text"
 | number == 3 = "red-text"
 | number >= 4 = "purple-text"

cellTextIndicator :: Location -> Board -> String
cellTextIndicator loc board
    | isDiscovered board loc && isBomb board loc = "💣"
    | isDiscovered board loc = 
      if (countBombNeighbors board loc > 0)
      then show $ countBombNeighbors board loc
      else ""
    | otherwise = ""