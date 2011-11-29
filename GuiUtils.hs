module GuiUtils where

import Graphics.UI.Gtk

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
newBoxButton :: (BoxClass a) => a -> String -> IO Button
newBoxButton box s = do
  button <- buttonNew
  set button [ buttonLabel := s ]
  addToBox box button
  return button

-- main gui loop
runGUI :: IO Window -> IO ()
runGUI gui = do
  initGUI
  window <- gui
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

