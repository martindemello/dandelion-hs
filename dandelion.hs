import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk as G hiding (Point)
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.Rendering.Cairo as C
import Control.Monad.Trans (liftIO)

type Editor = IORef [PairBox]

newEditor :: IO Editor
newEditor = newIORef []


data PairBox = Pair VBox

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
  return $ Pair box

viewBox (Pair v) = v

-- new textfield
makeEntry :: String -> IO Entry
makeEntry str = do
  e <- entryNew
  entrySetText e str
  return e

-- add widget to box with default params
addToBox box widget = boxPackStart box widget PackNatural 0

addPairToBox box v = addToBox box (viewBox v)

-- editor functions
addToEditor :: Editor -> PairBox -> IO Editor
addToEditor ed w = do
  es <- readIORef ed
  writeIORef ed (w : es)
  return ed

removeFromEditor :: Editor -> IO PairBox
removeFromEditor ed = do
  es <- readIORef ed
  e <- return $ head es
  writeIORef ed (tail es)
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
  return (es !! i)

getEntries :: Editor -> IO [PairBox]
getEntries ed = do
  es <- readIORef ed
  return es

-- main functions
addLine :: Editor -> EditorView -> String -> IO ()
addLine ed view s = do
  l <- addNewLine ed s
  addPairToBox (displayBox view) l

removeLine :: Editor -> EditorView -> IO ()
removeLine ed view = do
  es <- getEntries ed
  e <- removeFromEditor ed
  containerRemove (displayBox view) (viewBox e)

main :: IO ()
main = do
  initGUI
  window <- windowNew
  box <- vBoxNew False 0
  bbox <- hBoxNew False 0
  ebox <- vBoxNew False 0

  ev <- newIORef EditorView { displayBox = ebox, currentLine = 0 }

  G.on window G.keyPressEvent $ E.tryEvent $ do
    "Escape" <- E.eventKeyName
    C.liftIO $ G.widgetDestroy window

  set window [ containerChild := box ]

  addToBox box ebox
  addToBox box bbox

  plusButton <- buttonNew
  set plusButton [ buttonLabel := "Add" ]
  addToBox bbox plusButton

  minusButton <- buttonNew
  set minusButton [ buttonLabel := "Remove" ]
  addToBox bbox minusButton

  ed <- newEditor
  addLines ed 13

  eds <- getEntries ed
  mapM (addPairToBox ebox) eds

  view <- readIORef ev
  onClicked minusButton (removeLine ed view)
  onClicked plusButton (addLine ed view "" >> widgetShowAll ebox)

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
