import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk as G hiding (Point)
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.Rendering.Cairo as C
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Control.Monad.Trans (liftIO)

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

makePair :: String -> IO PairBox
makePair s = do
  box <- vBoxNew False 0
  label <- labelNew (Just s)
  entry <- makeEntry ""
  addToBox box label
  addToBox box entry
  return $ PairBox { pbOrig = label, pbText = entry, pbVbox = box }

-- new textfield
makeEntry :: String -> IO Entry
makeEntry str = do
  e <- entryNew
  entrySetText e str
  return e

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
  l <- makePair s
  addToEditor ed l
  return l

addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed "") [1..i])
  return ed

getLine :: Editor -> Int -> IO PairBox
getLine ed i = do
  es <- readIORef ed
  return (es ! i)

getPairs :: Editor -> IO (Vector PairBox)
getPairs ed = do
  es <- readIORef ed
  return es

getLines :: Editor -> IO [String]
getLines ed = do
  es <- getPairs ed
  ls <- V.mapM (entryGetText . pbText) es
  return $ V.toList ls

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

saveFile :: Editor -> String -> IO ()
saveFile ed path = do
  lines <- getLines ed
  writeFile path (unlines lines)

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
  saveButton <- newBoxButton bbox "Save"
  exitButton <- newBoxButton bbox "Exit"

  ed <- newEditor
  addLines ed 13

  eds <- getPairs ed
  V.mapM (addPairToBox ebox) eds

  view <- readIORef ev
  onClicked minusButton (removeLine ed view)
  onClicked plusButton (addLine ed view "" >> widgetShowAll ebox)
  onClicked saveButton (saveFile ed "file.out")
  onClicked exitButton (G.widgetDestroy window)

  return window
