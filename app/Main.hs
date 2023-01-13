{-# Language ScopedTypeVariables #-}
module Main (main) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny
import Reactive.Threepenny
import GameBoard
import IA
import System.Random ( newStdGen )

-- Data type representing the play mode
data PlayMode = Dig | Flag deriving (Show, Eq)

main :: IO ()
main = do
    -- Define the new custom events for reactive programming
    (boardEvent :: Event Board, boardHandler) <- newEvent
    (modeEvent :: Event (PlayMode -> PlayMode), modeHandler) <- newEvent
    startGUI defaultConfig { jsStatic = Just "./static" }
     (\window -> setup window boardEvent boardHandler modeEvent modeHandler)

setup :: Window -> Event Board -> Handler Board -> Event (PlayMode -> PlayMode) -> Handler (PlayMode -> PlayMode) -> UI ()
setup window boardEvent boardHandler modeEvent modeHandler = do
 -- Behaviors
 (boardBehavior :: Behavior Board) <- stepper emptyBoard boardEvent -- the initial board is an empty board
 (modeBehavior :: Behavior PlayMode) <- accumB Dig modeEvent -- the initial play mode is "dig"
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
 iterate2D 9 9 (\(x, y) -> cellUI wrapper gameStatus window boardHandler boardBehavior modeBehavior (x, y)) -- Grid's cell
 -- Mode button logic
 element mode # sink UI.text (modeToIndication <$> modeBehavior)
 on UI.click mode $ \_ -> do
    liftIO $ modeHandler toggleMode
 -- IA play button logic
 on UI.click iaPlay $ \_ -> do
     currentBoard <- currentValue boardBehavior
     let (bestMove, bestFlag) = findSafePlays currentBoard -- returns the best location to dig and best location to flag as maybes
     liftIO $ do
        case bestMove of
            Just move -> liftIO $ executePlayMove move gameStatus window boardHandler boardBehavior -- Best dig if present
            Nothing -> return ()
        case bestFlag of
            Just flag -> Main.toggleFlagCell flag boardHandler boardBehavior -- Best flag if present
            Nothing -> return ()
 -- Restart button logic
 on UI.click restart $ \_ -> do
    -- Trick to reset the game similar to a page refresh, delete the body and call setup again,
    -- finally set the board to empty board
    body <- getBody window
    UI.delete body
    liftIO $ boardHandler emptyBoard
    setup window boardEvent boardHandler modeEvent modeHandler
 -- Add all the UI elements to the body of the HTML page
 getBody window #+ [element gameTitle, element mode, element iaPlay, element restart, element gameStatus, element wrapper]
 return ()

-- Mapper from the PlayMode to an actual string indicating the mode
modeToIndication :: PlayMode -> String
modeToIndication mode = "Mode: " ++ show mode

-- Toggle the mode, given dig returns flag, given flag returns dig.
toggleMode :: PlayMode -> PlayMode
toggleMode playMode
    | playMode == Dig = Main.Flag
    | playMode == Main.Flag = Dig
    | otherwise = Dig

-- Iterate all the grid coordinates and apply the provided f function onto each coordinate.
iterate2D :: Int -> Int -> ((Int, Int) -> UI ()) -> UI ()
iterate2D rows cols f = mapM_ f [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]

-- The cell UI rendering and logic
cellUI :: Element -> Element -> Window -> Handler Board -> Behavior Board -> Behavior PlayMode -> Location -> UI ()
cellUI container gameStatus window boardHandler boardBehavior modeBehavior (x, y) = do
    -- Create a cell as a HTML div and add the default css class to the cell (hidden)
    cell <- UI.div
    -- Sink (bind) the class and text of the cell to the board's current value represented by a behavior
    element cell # sink UI.class_ ((cellClass (x, y)) <$> boardBehavior) -- mapper to have the cell class from the board and location
    element cell # sink UI.text ((cellTextIndicator (x, y)) <$> boardBehavior) -- mapper to have the cell text from the board and location
    -- Add the elements to the UI
    getBody window #+ [element cell]
    element container #+ [element cell]
    on UI.click cell $ \_ -> do
        -- Play the move corresponding to the cell's coordinates when clicked
        liftIO $ do
            currentMode <- currentValue modeBehavior
            if currentMode == Dig
                then executePlayMove (x, y) gameStatus window boardHandler boardBehavior -- Dig
                else Main.toggleFlagCell (x, y) boardHandler boardBehavior -- Flag

-- Function to handle the process of playing a move (calling the model, updating the UI...). This can be called by the human player or AI.
executePlayMove :: Location -> Element -> Window -> Handler Board -> Behavior Board -> IO ()
executePlayMove (x, y) gameStatus window boardHandler boardBehavior = do
     putStrLn $ "Playing cell " ++ show (x, y)
     gen <- newStdGen
     board <- currentValue boardBehavior
     let (newBoard, newPlayResult) = playMove board (x, y) gen
     runUI window $ element gameStatus # set UI.text ("Status : " ++ (show newPlayResult)) -- Execute an UI action to update the game status' text
     if newPlayResult == Won || newPlayResult == Lost 
        then boardHandler (revealBombs newBoard)  -- Game ended, reveal the bombs too before firing the new board
        else boardHandler newBoard -- Fire the new board

-- Toggle flag cell antechamber for the UI, retrieve the board from the behavior before calling the toggle flag implement in gameboard
toggleFlagCell :: Location -> Handler Board -> Behavior Board -> IO ()
toggleFlagCell loc boardHandler boardBehavior = do
    board <- currentValue boardBehavior
    putStrLn $ "Flagging " ++ show loc
    if not $ isDiscovered board loc -- You can only toggle flag on an undiscovered cell
        then boardHandler $ GameBoard.toggleFlagCell board loc -- Fire the new board
        else return ()

-- Get the class of a cell at the provided location on a given board
cellClass :: Location -> Board -> String
cellClass loc board = convertCellTypeToClassName $ getCellType board loc

-- Convert a cell type to a corresponding css class name for the UI
convertCellTypeToClassName :: CellType -> String
convertCellTypeToClassName cellType = case cellType of
 Hidden -> "cell hidden"
 Empty -> "cell shown"
 Number n -> "cell shown" ++ " " ++ numberColorClass n
 Bomb -> "cell bomb"
 GameBoard.Flag -> "cell flag"

-- Convert a number to a css class name to colorize the text accordingly
numberColorClass :: Int -> String
numberColorClass number
 | number == 1 = "blue-text"
 | number == 2 = "green-text"
 | number == 3 = "red-text"
 | number >= 4 = "purple-text"
 | otherwise = "blue-text"


-- Get the text indicator that should be displayed at a cell's location (nothing, a bomb, a flag, the number)
cellTextIndicator :: Location -> Board -> String
cellTextIndicator loc board = case cellType of
    Hidden -> ""
    Empty -> ""
    Number n -> show n
    Bomb -> "💣" -- bomb unicode char
    GameBoard.Flag -> "🚩" -- flag unicode char
    where cellType = getCellType board loc