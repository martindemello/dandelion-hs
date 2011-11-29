import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import Datafile
import GuiUtils

-- a PairBox contains a VBox containing a Label and an Entry
data PairBox = PairBox { pbOrig :: Label
                       , pbText :: Entry
                       , pbVbox :: VBox
                       }

type Editor = IORef (Vector PairBox)


newEditor :: IO Editor
newEditor = newIORef (V.empty)

data EditorView = EditorView { displayBox :: VBox
                             , currentLine :: Int
                             }

makePairBox :: (String, String) -> IO PairBox
makePairBox (l, s) = do
  box <- vBoxNew False 0
  label <- makeLabel l
  entry <- makeEntry s
  addToBox box label
  addToBox box entry
  return $ PairBox { pbOrig = label, pbText = entry, pbVbox = box }

origText :: PairBox -> IO String
origText pb = (labelGetText . pbOrig) pb

newText :: PairBox -> IO String
newText pb = (entryGetText . pbText) pb

pairOfPairBox :: PairBox -> IO (String, String)
pairOfPairBox pb = do
  a <- origText pb
  b <- newText pb
  return $ (a, b)

addPairToBox box v = addToBox box (pbVbox v)

-- editor functions
addToEditor :: Editor -> PairBox -> IO Editor
addToEditor ed w = do
  es <- readIORef ed
  writeIORef ed (es `V.snoc` w)
  return ed

removeFromEditor :: Editor -> IO PairBox
removeFromEditor ed = do
  es <- readIORef ed
  e <- return $ V.last es
  writeIORef ed (V.init es)
  return e

addNewLine :: Editor -> String -> IO PairBox
addNewLine ed s = do
  l <- makePairBox (s, "")
  addToEditor ed l
  return l

addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

getLine :: Editor -> Int -> IO PairBox
getLine ed i = do
  es <- readIORef ed
  return (es ! i)

-- just in case we change the internal representation of Editor
getPairs :: Editor -> IO (Vector PairBox)
getPairs ed = readIORef ed

getLines :: Editor -> IO [(String, String)]
getLines ed = do
  es <- getPairs ed
  ls <- V.mapM pairOfPairBox es
  return $ V.toList ls

-- file <-> editor

processFile :: (String -> [(String, String)]) -> Editor -> String -> IO ()
processFile f ed path = do
  s <- readFile path
  ps <- mapM makePairBox (f s)
  es <- return $ V.fromList ps
  writeIORef ed es

importFile = processFile parseImport
loadFile   = processFile parseFile

saveFile :: Editor -> String -> IO ()
saveFile ed path = do
  ls <- getLines ed
  writeFile path (showFile ls)

-- main functions
addLine :: Editor -> EditorView -> String -> IO ()
addLine ed view s = do
  l <- addNewLine ed s
  addPairToBox (displayBox view) l

removeLine :: Editor -> EditorView -> IO ()
removeLine ed view = do
  es <- getPairs ed
  e <- removeFromEditor ed
  containerRemove (displayBox view) (pbVbox e)

refreshView :: Editor -> EditorView -> IO ()
refreshView ed view = do
  es <- getPairs ed
  box <- return $ displayBox view
  containerForeach box (containerRemove box)
  V.mapM_ (addPairToBox box) es

runLoad :: Editor -> EditorView -> (Editor -> String -> IO ()) -> String -> IO ()
runLoad ed view fn path = do
  fn ed path
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

  eds <- getPairs ed
  V.mapM (addPairToBox ebox) eds

  view <- readIORef ev
  onClicked minusButton (removeLine ed view)
  onClicked plusButton (addLine ed view "" >> widgetShowAll ebox)
  onClicked loadButton (runLoad ed view loadFile "file.in" >> widgetShowAll window)
  onClicked saveButton (saveFile ed "file.out")
  onClicked exitButton (widgetDestroy window)
  onClicked importButton (runLoad ed view importFile "file.orig" >> widgetShowAll window)

  return window
