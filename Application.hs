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
import Types

-- scaffolding to test gui functionality during development
addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

-- add editor tab
addEditorTab :: Application -> IO EditorView
addEditorTab app = do
  ntbk <- return $ apNotebook app
  status <- return $ apStatus app
  ebox <- vBoxNew False 0

  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAlways
  scrolledWindowAddWithViewport scrwin ebox

  notebookAppendPage ntbk scrwin "[None]"

  ed <- newEditor
  addLines ed 3
  newEditorView app ed ebox scrwin status

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

  curview <- newIORef Nothing
  app <- return $ Application { apWindow = window
                              , apNotebook = ntbk
                              , apStatus = status
                              , apCurrentView = curview
                              }

  view <- addEditorTab app

  ui <- setupMenu app windowbox

  set window [ containerChild := windowbox ]

  boxPackS windowbox ntbk PackGrow 5
  addToBox windowbox sbar

  refreshView view
  writeIORef curview (Just view)
  return $ app
