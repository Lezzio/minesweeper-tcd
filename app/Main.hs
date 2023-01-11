module Main (main) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny

import Reactive.Threepenny
import System.FilePath(dropFileName)
import System.Environment(getExecutablePath)
import Debug.Trace(trace)

main :: IO ()
main = do
 startGUI defaultConfig { jsStatic = Just "./static" } setup

buttonUI :: Window -> UI ()
buttonUI window = do
    button <- UI.button #+ [string "Click me"]
    getBody window #+ [return button]
    on UI.click button  $\_ -> do
     getBody window #+ [ UI.div #+ [ string "clicked!"] ]

cellUI :: Window -> UI ()
cellUI window = do
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
 wrapper <- UI.div # set UI.id_ "minesweeper-wrapper"
 getBody window #+ [string "Hello, world!", return wrapper]
 buttonUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 cellUI window
 return ()