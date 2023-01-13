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
    (boardEvent :: Event Board, boardHandler) <- newEvent
    startGUI defaultConfig { jsStatic = Just "./static" } (\window -> setup window boardEvent boardHandler)

setup :: Window -> Event Board -> Handler Board -> UI ()
setup window boardEvent boardHandler = do
 -- Add the CSS stylesheet
 UI.addStyleSheet window "semantic.css"
 -- Declare the UI elements
 mode <- UI.button # set UI.text "Mode: "
 iaPlay <- UI.button # set UI.text "IA Play"
 restart <- UI.button # set UI.text "Restart game" # set UI.id_ "restart"
 wrapper <- UI.div # set UI.id_ "grid-wrapper"
 gameTitle <- UI.div # set UI.text "MINESWEEPER" # set UI.id_ "game-title"
 gameStatus <- UI.div # set UI.text "Status : Play your first move" # set UI.id_ "game-status"
 -- Grid rendering
 (boardBehavior :: Behavior Board) <- stepper emptyBoard boardEvent
 iterate2D 9 9 (\(x, y) -> cellUI wrapper gameStatus window boardHandler boardBehavior (x, y)) -- Grid's cell
 -- IA play button logic
 on UI.click iaPlay $ \_ -> do
     liftIO $ do
          executePlayMove (5, 5) gameStatus window boardHandler boardBehavior
 -- Restart button logic
 on UI.click restart $ \_ -> do
    -- Trick to reset the game similar to a page refresh, delete the body and call setup again,
    -- finally set the board to empty board
    body <- getBody window
    UI.delete body
    liftIO $ do
        boardHandler emptyBoard
    setup window boardEvent boardHandler

 getBody window #+ [element gameTitle, element mode, element iaPlay, element restart, element gameStatus, element wrapper]
 return ()

iterate2D :: Int -> Int -> ((Int, Int) -> UI ()) -> UI ()
iterate2D rows cols f = mapM_ f [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]

cellUI :: Element -> Element -> Window -> Handler Board -> Behavior Board -> Location -> UI ()
cellUI container gameStatus window boardHandler boardBehavior (x, y) = do
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
          executePlayMove (x, y) gameStatus window boardHandler boardBehavior

executePlayMove :: Location -> Element -> Window -> Handler Board -> Behavior Board -> IO ()
executePlayMove (x, y) gameStatus window boardHandler boardBehavior = do
     putStrLn $ "Playing cell " ++ show (x, y)
     gen <- newStdGen
     board <- currentValue boardBehavior
     let (newBoard, newPlayResult) = playMove board (x, y) gen
     runUI window $ do
        element gameStatus # set UI.text ("Status : " ++ (show newPlayResult))
     --gameStatus # set UI.text "Status : " ++ (show newPlayResult)
     if newPlayResult == Won || newPlayResult == Lost 
        then boardHandler (revealBombs newBoard) 
        else boardHandler newBoard

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
 | otherwise = "blue-text"

cellTextIndicator :: Location -> Board -> String
cellTextIndicator loc board
    | isDiscovered board loc && isBomb board loc = "💣"
    | isDiscovered board loc = 
      if (countBombNeighbors board loc > 0)
      then show $ countBombNeighbors board loc
      else ""
    | otherwise = ""