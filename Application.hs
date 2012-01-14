module Application where

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

data Application = Application { apWindow      :: Window
                               , apNotebook    :: Notebook
                               , apStatus      :: Label
                               , apCurrentView :: IORef EditorView
                               }

-- scaffolding to test gui functionality during development
addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

-- add editor tab
addEditorTab :: Notebook -> Label -> IO EditorView
addEditorTab ntbk status = do
  ebox <- vBoxNew False 0

  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAlways
  scrolledWindowAddWithViewport scrwin ebox

  notebookAppendPage ntbk scrwin "[None]"

  ed <- newEditor
  addLines ed 3
  newEditorView ed ebox ntbk scrwin status

-- newWindow
makeApplication :: IO Application
makeApplication = do
  window <- windowNew
  set window [windowTitle := "Dandelion"]

  windowbox <- vBoxNew False 0

  ntbk <- notebookNew
  set ntbk [notebookScrollable := True, notebookTabPos := PosTop]
  sbar <- hBoxNew False 0
  status <- makeLabel ""
  addToBox sbar status

  view <- addEditorTab ntbk status

  ui <- setupMenu window windowbox view

  set window [ containerChild := windowbox ]

  boxPackS windowbox ntbk PackGrow 5
  addToBox windowbox sbar
  curview <- newIORef view

  refreshView view
  return $ Application { apWindow = window
                       , apNotebook = ntbk
                       , apStatus = status
                       , apCurrentView = curview
                       }
    

