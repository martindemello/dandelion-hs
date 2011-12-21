module TopLevel where

import Graphics.UI.Gtk
import System.FilePath
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO
import Data.IORef

data EditorView = EditorView { evEditor :: IORef Editor
                             , displayBox :: VBox
                             , noteBook :: Notebook
                             , scrollPane :: ScrolledWindow
                             , status :: Label
                             , currentLine :: IORef Int
                             }

newEditorView :: Editor -> VBox -> Notebook -> ScrolledWindow
  -> Label -> HBox -> IO EditorView
newEditorView ed ebox ntbk swin status sbar = do
  lnum  <- newIORef 0
  ie <- newIORef ed
  return $ EditorView { evEditor = ie
                      , displayBox = ebox
                      , noteBook = ntbk
                      , scrollPane = swin
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
  refreshStatus view

refreshStatus :: EditorView -> IO ()
refreshStatus view =
  setStatus view >>
  setNotebookTabLabel view

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
  refreshStatus view
  widgetShowAll window

runSaveFile = runSave False
runSaveFileAs = runSave True

runExportFile :: Window -> EditorView -> IO ()
runExportFile window view = do
  ed <- getEditor view
  fileExportDialog window (exportFile ed)

setLine :: EditorView -> Int -> IO ()
setLine view i = do
  writeIORef (currentLine view) i
  setStatus view

showFilename :: Maybe String -> String
showFilename Nothing = "[None]"
showFilename (Just s) = s

statusLine :: EditorView -> IO String
statusLine view = do
  ed <- getEditor view
  f <- readIORef $ edFilename ed
  i <- readIORef $ currentLine view
  return $ "File: " ++ (showFilename f) ++ " Line: " ++ show (i + 1)

setStatus :: EditorView -> IO ()
setStatus view = do
  s <- return $ status view
  text <- statusLine view
  labelSetText s text

setNotebookTabLabel :: EditorView -> IO ()
setNotebookTabLabel view = do
  nb <- return $ toNotebook $ noteBook view
  sp <- return $ scrollPane view
  ed <- getEditor view
  f <- readIORef $ edFilename ed
  notebookSetTabLabelText nb sp (takeFileName $ showFilename f)
