module TopLevel where

import Graphics.UI.Gtk
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO

data EditorView = EditorView { displayBox :: VBox
                             , currentLine :: Int
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
runLoad win ed view fn = do
  fch <- fileOpenDialog win (fn ed)
  refreshView ed view

