import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V

import Datafile
import Editor
import EditorView
import FileIO
import GuiUtils
import Menu
import Packable

-- add editor tab
addEditorTab :: Notebook -> Label -> Editor -> IO EditorView
addEditorTab ntbk status ed = do
  ebox <- vBoxNew False 0

  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAlways
  scrolledWindowAddWithViewport scrwin ebox

  notebookAppendPage ntbk scrwin "[None]"

  newEditorView ed ebox ntbk scrwin status


-- scaffolding to test gui functionality during development
addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)

main :: IO ()
main = runGUI $ do
  window <- windowNew
  set window [windowTitle := "Dandelion"]

  box <- vBoxNew False 0

  ntbk <- notebookNew
  set ntbk [notebookScrollable := True, notebookTabPos := PosTop]

  ed <- newEditor

  sbar <- hBoxNew False 0
  status <- makeLabel ""
  addToBox sbar status

  sbar <- hBoxNew False 0
  status <- makeLabel ""
  addToBox sbar status

  view <- addEditorTab ntbk status ed

  ui <- setupMenu window box ed view

  set window [ containerChild := box ]

  boxPackS box ntbk PackGrow 5
  addToBox box sbar

  addLines ed 3
  refreshView view

  return window
