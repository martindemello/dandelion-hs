import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO
import Menu
import EditorView

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
  ebox <- vBoxNew False 0

  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAlways
  scrolledWindowAddWithViewport scrwin ebox

  notebookAppendPage ntbk scrwin "[None]"

  sbar <- hBoxNew False 0
  status <- makeLabel ""
  addToBox sbar status

  view <- newEditorView ed ebox ntbk scrwin status sbar

  ui <- setupMenu window box ed view

  set window [ containerChild := box ]

  boxPackS box ntbk PackGrow 5
  addToBox box sbar

  addLines ed 3
  refreshView view

  return window
