import Graphics.UI.Gtk

import Application
import GuiUtils
import Packable
import Menu
import Types

mainWindow :: IO Application
mainWindow = do
  window <- windowNew
  set window [windowTitle := "Dandelion"]
  windowbox <- vBoxNew False 0
  set window [ containerChild := windowbox ]

  ntbk <- notebookNew
  set ntbk [notebookScrollable := True, notebookTabPos := PosTop]

  sbar <- hBoxNew False 0
  status <- makeLabel ""
  addToBox sbar status

  app <- makeApplication window ntbk status
  ui <- setupMenu app windowbox

  boxPackS windowbox ntbk PackGrow 5
  addToBox windowbox sbar
  return app


main :: IO ()
main = runGUI $ do
  app <- mainWindow
  return $ apWindow app
