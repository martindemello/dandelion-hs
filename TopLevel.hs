module TopLevel where

import Graphics.UI.Gtk
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO
import Data.IORef

data EditorView = EditorView { evEditor :: IORef Editor
                             , displayBox :: VBox
                             , status :: Label
                             , currentLine :: IORef Int
                             }

newEditorView :: Editor -> VBox -> Label -> HBox -> IO EditorView
newEditorView ed ebox status sbar = do
  lnum  <- newIORef 0
  ie <- newIORef ed
  return $ EditorView { evEditor = ie
                      , displayBox = ebox
                      , status = status
                      , currentLine = lnum
                      }

getEditor :: EditorView -> IO Editor
getEditor view = readIORef $ evEditor view

addLine :: EditorView -> String -> IO ()
addLine view s = do
  ed <- getEditor view
  l <- addNewLine ed s
  addPairToBox (displayBox view) l

removeLine :: EditorView -> IO ()
removeLine view = do
  ed <- getEditor view
  es <- getContent ed
  e <- removeFromEditor ed
  containerRemove (displayBox view) (pbVbox e)

addFocusHandler :: EditorView -> Int -> IO ()
addFocusHandler view i = do
  ed <- getEditor view
  p <- Editor.getLine ed i
  e <- return $ pbText p
  onFocusIn e $ \dirtype -> setLine view i >> return False
  return ()

refreshView :: EditorView -> IO ()
refreshView view = do
  ed <- getEditor view
  es <- getContent ed
  box <- return $ displayBox view
  containerForeach box (containerRemove box)
  V.mapM_ (addPairToBox box) es
  mapM_ (addFocusHandler view) [0 .. (V.length es - 1)]
  setStatus view

runLoad :: (Editor -> String -> IO ()) -> Window -> EditorView -> IO ()
runLoad fn window view = do
  ed <- getEditor view
  fch <- fileOpenDialog window (fn ed)
  refreshView view
  widgetShowAll window

runLoadFile = runLoad loadFile
runImportFile = runLoad importFile

runSave :: Bool -> Window -> EditorView -> IO ()
runSave newFile window view = do
  ed <- getEditor view
  f <- readIORef $ edFilename ed
  case (newFile, f) of
       (False, Just path) -> saveFile ed path
       _ -> fileSaveDialog window (saveFile ed)

runSaveFile = runSave False
runSaveFileAs = runSave True

setFilename :: EditorView -> String -> IO ()
setFilename view fname = do
  ed <- getEditor view
  writeIORef (edFilename ed) (Just fname)
  setStatus view

setLine :: EditorView -> Int -> IO ()
setLine view i = do
  writeIORef (currentLine view) i
  setStatus view

showFilename :: Maybe String -> String
showFilename Nothing = "[None]"
showFilename (Just s) = s

setStatus :: EditorView -> IO ()
setStatus view = do
  ed <- getEditor view
  s <- return $ status view
  f <- readIORef $ edFilename ed
  i <- readIORef $ currentLine view
  labelSetText s ("File: " ++ (showFilename f) ++ " Line: " ++ show (i + 1))
