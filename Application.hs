module Application where

import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V
import Control.Monad.TM

import Datafile
import Editor
import EditorView
import FileIO
import GuiUtils
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
makeApplication :: Window -> Notebook -> Label -> IO Application
makeApplication window ntbk status = do
  views <- newIORef []
  app <- return $ Application { apWindow = window
                              , apNotebook = ntbk
                              , apStatus = status
                              , apViews = views
                              }

  view1 <- addEditorTab app
  view2 <- addEditorTab app

  refreshView view1
  writeIORef views [view1, view2]
  return $ app

isCurrentView :: Notebook -> Int -> EditorView -> IO Bool
isCurrentView ntbk tab ev = do
  n <- get ntbk $ notebookChildPosition (scrollPane ev)
  return (n == tab)

getCurrentView :: Application -> IO (Maybe EditorView)
getCurrentView app = do
  ntbk <- return $ apNotebook app
  tab <- notebookGetCurrentPage ntbk
  evs <- readIORef (apViews app)
  findM (isCurrentView ntbk tab) evs
