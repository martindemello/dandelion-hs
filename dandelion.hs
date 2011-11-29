import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk as G hiding (Point)
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.Rendering.Cairo as C
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Text.JSON as JSON
import Data.Vector (Vector, (!))
import Control.Monad.Trans (liftIO)
import Data.String.Utils

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

makePair :: (String, String) -> IO PairBox
makePair (l, s) = do
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

stringOfPair :: PairBox -> IO String
stringOfPair pb = do
  a <- origText pb
  b <- newText pb
  return ("# " ++ a ++ "\n> " ++ b)

-- new textfield
makeEntry :: String -> IO Entry
makeEntry str = do
  e <- entryNew
  entrySetText e str
  return e

-- new label
makeLabel :: String -> IO Label
makeLabel s = do
  label <- labelNew (Just s)
  miscSetAlignment label 0 0
  return label

-- add widget to box with default params
addToBox box widget = boxPackStart box widget PackNatural 0

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
  l <- makePair (s, "")
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

getLines :: Editor -> IO [String]
getLines ed = do
  es <- getPairs ed
  ls <- V.mapM stringOfPair es
  return $ V.toList ls

-- file <-> editor

processFile :: (String -> [a]) -> (a -> IO PairBox) -> Editor -> String -> IO ()
processFile f g ed path = do
  s <- readFile path
  ps <- mapM g (f s)
  es <- return $ V.fromList ps
  writeIORef ed es

importFile = processFile lines (\x -> makePair (x, ""))
loadFile   = processFile parseFile makePair

saveFile :: Editor -> String -> IO ()
saveFile ed path = do
  lines <- getLines ed
  writeFile path (unlines lines)

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

joinLines :: [String] -> String
joinLines [] = ""
joinLines [x] = strip $ tail x
joinLines (x : xs) = (strip $ tail x) ++ "\n" ++ (joinLines xs)

collectLines :: [String] -> [String]
collectLines ls = map joinLines $ L.groupBy (\x y -> head x == head y) ls 

parseLines :: [String] -> [(String, String)] -> [(String, String)]
parseLines [] a = a
parseLines [x] a = (x, "") : a
parseLines (x : y : xs) a = parseLines xs ((x, y) : a)

parseFile :: String -> [(String, String)]
parseFile s = reverse $ parseLines (collectLines $ lines s) []

refreshView :: Editor -> EditorView -> IO ()
refreshView ed view = do
  es <- getPairs ed
  box <- return $ displayBox view
  containerForeach box (containerRemove box)
  V.mapM_ (addPairToBox box) es

runLoad :: Editor -> IORef EditorView -> (Editor -> String -> IO ()) -> String -> IO ()
runLoad ed ev fn path = do
  view <- readIORef ev
  fn ed path
  refreshView ed view

newBoxButton :: (BoxClass a) => a -> String -> IO Button
newBoxButton box s = do
  button <- buttonNew
  set button [ buttonLabel := s ]
  addToBox box button
  return button

runGUI :: IO Window -> IO ()
runGUI gui = do
  initGUI
  window <- gui
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

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
  onClicked loadButton (runLoad ed ev loadFile "file.in" >> widgetShowAll window)
  onClicked saveButton (saveFile ed "file.out")
  onClicked exitButton (G.widgetDestroy window)
  onClicked importButton (runLoad ed ev importFile "file.orig" >> widgetShowAll window)

  return window
