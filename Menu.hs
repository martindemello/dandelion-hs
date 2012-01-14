module Menu where

import Graphics.UI.Gtk
import Data.IORef

import Datafile
import GuiUtils
import Editor
import FileIO
import EditorView
import Types

-- debugging helper
prAct :: ActionClass self => self -> IO (ConnectId self)
prAct a = onActionActivate a $ do name <- actionGetName a
                                  putStrLn ("Action Name: " ++ name)

makeMenu :: Window -> VBox -> ActionGroup -> IO UIManager
makeMenu window box agr = do
  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiDecl
  uiManagerInsertActionGroup ui agr 0
  acg <- uiManagerGetAccelGroup ui
  windowAddAccelGroup window acg

  addMenubarActions agr
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
  return ui

-- menubar actions

addMenubarAction action text =
  actionNew action text Nothing Nothing

addMenubarActions :: ActionGroup -> IO ()
addMenubarActions agr = do
  fma <- addMenubarAction "FMA" "_File"
  ema <- addMenubarAction "EMA" "_Edit"
  hma <- addMenubarAction "HMA" "_Help"
  mapM_ (actionGroupAddAction agr) [fma, ema, hma]

-- menu actions

doWithCurrentView :: Application -> MenuFileOp -> IO ()
doWithCurrentView app fn = do
  evr <- readIORef $ apCurrentView app
  window <- return $ apWindow app
  case evr of
       Nothing -> return ()
       Just ev -> fn window ev

setupMenu :: Application -> VBox -> IO UIManager
setupMenu app box = do
  -- define menus
  impa <- actionNew "IMPA" "_Import"  (Just "Import file") (Just stockNew)
  opna <- actionNew "OPNA" "_Open"    (Just "Open file") (Just stockOpen)
  sava <- actionNew "SAVA" "_Save"    (Just "Save file") (Just stockSave)
  svaa <- actionNew "SVAA" "Save _As" (Just "Save file as") (Just stockSaveAs)
  expa <- actionNew "EXPA" "_Export"  (Just "Export file") (Just stockSaveAs)
  exia <- actionNew "EXIA" "E_xit"    (Just "Exit") (Just stockQuit)

  cuta <- actionNew "CUTA" "Cu_t"   (Just "Cut") (Just stockCut)
  copa <- actionNew "COPA" "_Copy"  (Just "Copy") (Just stockCopy)
  psta <- actionNew "PSTA" "_Paste" (Just "Paste") (Just stockPaste)

  hlpa <- actionNew "HLPA" "_Help"  (Just "Help") (Just stockHelp)

  window <- return $ apWindow app
  -- file menu
  let fDo = doWithCurrentView app
  onActionActivate exia (widgetDestroy window)
  onActionActivate opna (fDo runLoadFile)
  onActionActivate sava (fDo runSaveFile)
  onActionActivate svaa (fDo runSaveFileAs)
  onActionActivate impa (fDo runImportFile)
  onActionActivate expa (fDo runExportFile)

  agr <- actionGroupNew "AGR"
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing)
    [impa,opna,sava,svaa,expa,exia,cuta,copa,psta,hlpa]

  ui <- makeMenu window box agr
  return ui

-- Menu

uiDecl = "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"IMPA\" />\
\              <menuitem action=\"OPNA\" />\
\              <menuitem action=\"SAVA\" />\
\              <menuitem action=\"SVAA\" />\
\              <menuitem action=\"EXPA\" />\
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
\            <toolitem action=\"EXPA\" />\
\            <toolitem action=\"EXIA\" />\
\            <separator />\
\            <toolitem action=\"CUTA\" />\
\            <toolitem action=\"COPA\" />\
\            <toolitem action=\"PSTA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

