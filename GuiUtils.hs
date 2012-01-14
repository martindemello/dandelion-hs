module GuiUtils where

import Graphics.UI.Gtk
import Packable

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

newBoxButton :: (BoxClass a) => a -> String -> IO Button
newBoxButton box s = do
  button <- buttonNew
  set button [ buttonLabel := s ]
  addToBox box button
  return button

-- file chooser dialogs (open, save, import, export)

runFileDialog :: FileChooserDialog -> Window -> (String -> IO ()) -> IO ()
runFileDialog fch win cb = do
  response <- dialogRun fch
  case response of
       ResponseAccept -> do
         Just path <- fileChooserGetFilename fch
         cb path
       ResponseCancel -> return ()
       ResponseDeleteEvent -> return ()
  widgetDestroy fch

fileOpenDialog :: Window -> (String -> IO ()) -> IO ()
fileOpenDialog win cb = do
  fch <- fileChooserDialogNew (Just "Open File")
                              (Just win)
                              FileChooserActionOpen
                              [("gtk-open",ResponseAccept), ("gtk-cancel",ResponseCancel)]
  runFileDialog fch win cb

fileSaveExportDialog :: String -> Window -> (String -> IO ()) -> IO ()
fileSaveExportDialog title win cb = do
  fch <- fileChooserDialogNew (Just title)
                              (Just win)
                              FileChooserActionSave
                              [("gtk-save",ResponseAccept), ("gtk-cancel",ResponseCancel)]
  runFileDialog fch win cb

fileSaveDialog = fileSaveExportDialog "Save File"
fileExportDialog = fileSaveExportDialog "Export File"

-- main gui loop
runGUI :: IO Window -> IO ()
runGUI gui = do
  initGUI
  window <- gui
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
