module Main (main) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny
import Reactive.Threepenny

main :: IO ()
main = do
 startGUI defaultConfig { jsStatic = Just "./static" } setup

cellUI :: Window -> Int -> Int -> UI ()
cellUI window x y = do
    -- Create a cell as a HTML div
    -- add the default css class to the cell
    cell <- UI.div
    element cell #. "cell hidden"
    getBody window #+ [return cell]
    container <- getElementById window "minesweeper-wrapper"
    case container of 
      Just x -> (element x) #+ [return cell]
      Nothing -> getBody window #+ [string "Nothing!"]
    on UI.click cell $ \event -> do
     element cell #. "cell empty"
     {-element cell # set UI.text "3"-}
     getBody window #+ [string "Left Click detected!"]

setup :: Window -> UI ()
setup window = do
 UI.addStyleSheet window "semantic.css"
 button <- UI.button # set UI.text "Okay"
 wrapper <- UI.div # set UI.id_ "minesweeper-wrapper"
 getBody window #+ [string "Hello, world!", return wrapper, return button]
 iterate2D 9 9 (\(x, y) -> cellUI window x y)
 return ()

iterate2D :: Int -> Int -> ((Int, Int) -> UI ()) -> UI ()
iterate2D rows cols f = mapM_ f [(x,y) | x <- [0..rows-1], y <- [0..cols-1]]