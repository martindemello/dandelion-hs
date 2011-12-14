module TopLevel where

import Graphics.UI.Gtk
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO
import Data.IORef

data EditorView = EditorView { displayBox :: VBox
                             , status :: Label
                             , currentLine :: IORef Int
                             , fileName :: IORef (Maybe String)
                             }

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

runLoad :: (Editor -> String -> IO ()) -> Window -> Editor -> EditorView -> IO ()
runLoad fn window ed view = do
  fch <- fileOpenDialog window (fn ed)
  refreshView ed view
  widgetShowAll window

runLoadFile = runLoad loadFile
runImportFile = runLoad importFile

runSave :: Bool -> Window -> Editor -> EditorView -> IO ()
runSave newFile window ed view = do
  f <- readIORef $ fileName view
  case (newFile, f) of
       (False, Just path) -> saveFile ed path
       _ -> fileSaveDialog window (saveFile ed)

runSaveFile = runSave False
runSaveFileAs = runSave True

setFilename :: EditorView -> String -> IO ()
setFilename ev fname = do
  writeIORef (fileName ev) (Just fname)
  setStatus ev 

setLine :: EditorView -> Int -> IO ()
setLine ev i = do
  writeIORef (currentLine ev) i
  setStatus ev 

showFilename :: Maybe String -> String
showFilename Nothing = "[None]"
showFilename (Just s) = s

setStatus :: EditorView -> IO ()
setStatus ev = do
  s <- return $ status ev
  f <- readIORef $ fileName ev
  i <- readIORef $ currentLine ev
  labelSetText s ("File: " ++ (showFilename f) ++ " Line: " ++ show (i + 1)) 
