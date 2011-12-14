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

addFocusHandler :: Editor -> EditorView -> Int -> IO ()
addFocusHandler ed ev i = do
  p <- Editor.getLine ed i
  e <- return $ pbText p
  onFocusIn e $ \dirtype -> setLine ev i >> return False
  return ()


main :: IO ()
main = runGUI $ do
  window <- windowNew
  box <- vBoxNew False 0
  ed <- newEditor
  ebox <- vBoxNew False 0
  sbar <- hBoxNew False 0
  status <- makeLabel ""
  lnum  <- newIORef 0
  fname <- newIORef Nothing
  view <- return $ EditorView { displayBox = ebox
                              , status = status
                              , currentLine = lnum
                              , fileName = fname
                              }
  setStatus view

  ui <- setupMenu window box ed view

  set window [ containerChild := box ]

  addToBox sbar status
  addToBox box ebox
  addToBox box sbar

  addLines ed 13

  eds <- getContent ed
  V.mapM (addPairToBox ebox) eds

  mapM_ (addFocusHandler ed view) [0 .. (V.length eds - 1)]

  return window
