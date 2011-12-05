module TopLevel where

import Graphics.UI.Gtk
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO

data EditorView = EditorView { displayBox :: VBox
                             , currentLine :: Int
                             , fileName :: Maybe String
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

runLoad :: Window -> Editor -> EditorView -> (Editor -> String -> IO ()) -> IO ()
runLoad window ed view fn = do
  fch <- fileOpenDialog window (fn ed)
  refreshView ed view
  widgetShowAll window

runSave :: Window -> Editor -> EditorView -> Bool -> IO ()
runSave window ed view newFile =
  case (newFile, fileName view) of
       (False, Just path) -> saveFile ed path
       _ -> fileSaveDialog window (saveFile ed)

