import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO

data EditorView = EditorView { displayBox :: VBox
                             , currentLine :: Int
                             }

-- scaffolding to test gui functionality during development
addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

-- main functions
addLine :: Editor -> EditorView -> String -> IO ()
addLine ed view s = do
  l <- addNewLine ed s
  addPairToBox (displayBox view) l

removeLine :: Editor -> EditorView -> IO ()
removeLine ed view = do
  es <- getContent ed
  e <- removeFromEditor ed
  containerRemove (displayBox view) (pbVbox e)

refreshView :: Editor -> EditorView -> IO ()
refreshView ed view = do
  es <- getContent ed
  box <- return $ displayBox view
  containerForeach box (containerRemove box)
  V.mapM_ (addPairToBox box) es

runLoad :: Window -> Editor -> EditorView -> (Editor -> String -> IO ()) -> IO ()
runLoad win ed view fn = do
  fch <- fileOpenDialog win
  response <- dialogRun fch
  case response of
       ResponseAccept -> do
         Just path <- fileChooserGetFilename fch
         fn ed path
       ResponseCancel -> return ()
       ResponseDeleteEvent -> return ()
  widgetDestroy fch
  refreshView ed view

main :: IO ()
main = runGUI $ do
  window <- windowNew
  box <- vBoxNew False 0
  bbox <- hBoxNew False 0
  ebox <- vBoxNew False 0

  ev <- newIORef EditorView { displayBox = ebox, currentLine = 0 }

  set window [ containerChild := box ]

  addToBox box ebox
  addToBox box bbox

  plusButton <- newBoxButton bbox "Add"
  minusButton <- newBoxButton bbox "Remove"
  loadButton <- newBoxButton bbox "Load"
  saveButton <- newBoxButton bbox "Save"
  importButton <- newBoxButton bbox "Import"
  exitButton <- newBoxButton bbox "Exit"

  ed <- newEditor
  addLines ed 13

  eds <- getContent ed
  V.mapM (addPairToBox ebox) eds

  view <- readIORef ev
  onClicked minusButton (removeLine ed view)
  onClicked plusButton (addLine ed view "" >> widgetShowAll ebox)
  onClicked loadButton (runLoad window ed view loadFile >> widgetShowAll window)
  onClicked saveButton (saveFile ed "file.out")
  onClicked exitButton (widgetDestroy window)
  onClicked importButton (runLoad window ed view importFile >> widgetShowAll window)

  return window
