import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO
import Menu
import TopLevel

-- scaffolding to test gui functionality during development
addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)

addFocusHandler :: Editor -> Int -> IO ()
addFocusHandler ed i = do
  p <- Editor.getLine ed i
  e <- return $ pbText p
  onFocusIn e $ \dirtype -> putStrLn ("focused" ++ show i) >> return False
  return ()


main :: IO ()
main = runGUI $ do
  window <- windowNew
  box <- vBoxNew False 0
  ed <- newEditor
  ebox <- vBoxNew False 0
  ev <- newIORef EditorView { displayBox = ebox, currentLine = 0 }
  view <- readIORef ev
  ui <- setupMenu window box ed view

  set window [ containerChild := box ]

  addToBox box ebox

  addLines ed 13

  eds <- getContent ed
  V.mapM (addPairToBox ebox) eds

  mapM_ (addFocusHandler ed) [0 .. (V.length eds - 1)]

  return window
