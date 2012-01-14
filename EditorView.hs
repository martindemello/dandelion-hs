{-# LANGUAGE MultiParamTypeClasses #-}

module EditorView where

import Graphics.UI.Gtk
import System.FilePath
import Data.IORef
import qualified Data.Vector as V

import Datafile
import Editor
import FileIO
import GuiUtils
import Packable
import Types

instance Packable EditorView VBox where
    widgetOf = displayBox

data PairView = PairView { pvPairBox :: PairBox
                         , pvMargin  :: Label
                         , pvBox     :: HBox
                         }

instance Packable PairView HBox where
    widgetOf = pvBox

newPairView :: PairBox -> Int -> IO PairView
newPairView pb i = do
  hbox <- hBoxNew False 0
  label <- makeLabel (show i)
  boxPackS hbox label PackNatural 3
  boxPackS hbox pb PackGrow 1
  return $ PairView { pvPairBox = pb
                    , pvMargin  = label
                    , pvBox     = hbox
                    }

newEditorView :: Application -> Editor -> VBox -> ScrolledWindow
  -> Label -> IO EditorView
newEditorView app ed ebox swin status = do
  lnum  <- newIORef 0
  ie <- newIORef ed
  return $ EditorView { evEditor = ie
                      , evApp = app
                      , displayBox = ebox
                      , scrollPane = swin
                      , status = status
                      , currentLine = lnum
                      }

getEditor :: EditorView -> IO Editor
getEditor view = readIORef $ evEditor view

addLine :: EditorView -> String -> IO ()
addLine view s = do
  ed <- getEditor view
  pb <- addNewLine ed s
  addPairToBox (displayBox view) pb

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

-- helper function for refreshView
addPairToViewBox :: VBox -> (PairBox, Int) -> IO ()
addPairToViewBox box (pb, i) = do
  pv <- newPairView pb i
  addToBox box pv

refreshView :: EditorView -> IO ()
refreshView view = do
  ed <- getEditor view
  es <- getContent ed
  box <- return $ displayBox view
  containerForeach box (containerRemove box)
  let n = V.length es
  mapM_ (addPairToViewBox box) (zip (V.toList es) [1 .. n])
  mapM_ (addFocusHandler view) [0 .. (n - 1)]
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
  f <- readIORef $ edFilePath ed
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

showFilePath :: Maybe String -> String
showFilePath Nothing = "[None]"
showFilePath (Just s) = s

statusLine :: EditorView -> IO String
statusLine view = do
  ed <- getEditor view
  f <- readIORef $ edFilePath ed
  i <- readIORef $ currentLine view
  return $ "File: " ++ (showFilePath f) ++ " Line: " ++ show (i + 1)

setStatus :: EditorView -> IO ()
setStatus view = do
  s <- return $ status view
  text <- statusLine view
  labelSetText s text

setNotebookTabLabel :: EditorView -> IO ()
setNotebookTabLabel view = do
  nb <- return $ toNotebook $ apNotebook $ evApp view
  sp <- return $ scrollPane view
  ed <- getEditor view
  f <- readIORef $ edFilePath ed
  notebookSetTabLabelText nb sp (takeFileName $ showFilePath f)
