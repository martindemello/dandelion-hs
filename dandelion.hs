import Graphics.UI.Gtk
import Data.IORef
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import qualified Data.Vector as V

import Datafile
import GuiUtils
import Editor
import FileIO

data EditorView = EditorView { displayBox :: VBox
                             , currentLine :: Int
                             }

-- scaffolding to test gui functionality during development
addLines :: Editor -> Int -> IO Editor
addLines ed i = do
  sequence_ (map (\x -> addNewLine ed (" hello " ++ (show x))) [1..i])
  return ed

prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)

-- main functions
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

main :: IO ()
main = runGUI $ do
  window <- windowNew
  box <- vBoxNew False 0

  fma <- actionNew "FMA" "File" Nothing Nothing
  ema <- actionNew "EMA" "Edit" Nothing Nothing
  hma <- actionNew "HMA" "Help" Nothing Nothing

  impa <- actionNew "IMPA" "Import"  (Just "Import file") (Just stockNew)
  opna <- actionNew "OPNA" "Open"    (Just "Open file") (Just stockOpen)
  sava <- actionNew "SAVA" "Save"    (Just "Save file") (Just stockSave)
  svaa <- actionNew "SVAA" "Save As" (Just "Save file as") (Just stockSaveAs)
  exia <- actionNew "EXIA" "Exit"    (Just "Exit") (Just stockQuit)

  cuta <- actionNew "CUTA" "Cut"   (Just "Cut") (Just stockCut)
  copa <- actionNew "COPA" "Copy"  (Just "Copy") (Just stockCopy)
  psta <- actionNew "PSTA" "Paste" (Just "Paste") (Just stockPaste)

  hlpa <- actionNew "HLPA" "Help"  (Just "Help") (Just stockHelp)

  agr <- actionGroupNew "AGR"
  mapM_ (actionGroupAddAction agr) [fma, ema, hma]
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing)
    [impa,opna,sava,svaa,cuta,copa,psta,hlpa]

  actionGroupAddActionWithAccel agr exia (Just "<Control>e")

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiDecl
  uiManagerInsertActionGroup ui agr 0

  maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
  let menubar = case maybeMenubar of
                     (Just x) -> x
                     Nothing -> error "Cannot get menubar from string."
  boxPackStart box menubar PackNatural 0

  maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
  let toolbar = case maybeToolbar of
                     (Just x) -> x
                     Nothing -> error "Cannot get toolbar from string."
  boxPackStart box toolbar PackNatural 0

  actionSetSensitive cuta False

  mapM_ prAct [svaa,cuta,copa,psta,hlpa]

  ebox <- vBoxNew False 0

  ev <- newIORef EditorView { displayBox = ebox, currentLine = 0 }

  set window [ containerChild := box ]

  addToBox box ebox

  ed <- newEditor
  addLines ed 13

  eds <- getContent ed
  V.mapM (addPairToBox ebox) eds

  view <- readIORef ev

  onActionActivate exia (widgetDestroy window)
  onActionActivate opna (runLoad window ed view loadFile >> widgetShowAll window)
  onActionActivate sava (saveFile ed "file.out")
  onActionActivate impa (runLoad window ed view importFile >> widgetShowAll window)

  return window

uiDecl=  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"IMPA\" />\
\              <menuitem action=\"OPNA\" />\
\              <menuitem action=\"SAVA\" />\
\              <menuitem action=\"SVAA\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\           <menu action=\"EMA\">\
\              <menuitem action=\"CUTA\" />\
\              <menuitem action=\"COPA\" />\
\              <menuitem action=\"PSTA\" />\
\           </menu>\
\            <separator />\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"IMPA\" />\
\            <toolitem action=\"OPNA\" />\
\            <toolitem action=\"SAVA\" />\
\            <toolitem action=\"EXIA\" />\
\            <separator />\
\            <toolitem action=\"CUTA\" />\
\            <toolitem action=\"COPA\" />\
\            <toolitem action=\"PSTA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

