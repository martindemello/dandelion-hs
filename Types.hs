module Types where

import Graphics.UI.Gtk
import Data.IORef
import Data.Vector

data Application = Application { apWindow      :: Window
                               , apNotebook    :: Notebook
                               , apStatus      :: Label
                               , apCurrentView :: IORef (Maybe EditorView)
                               }


data EditorView = EditorView { evEditor    :: IORef Editor
                             , evApp       :: Application
                             , displayBox  :: VBox
                             , scrollPane  :: ScrolledWindow
                             , status      :: Label
                             , currentLine :: IORef Int
                             }

data Editor = Editor { edPairs    :: IORef (Vector PairBox)
                     , edFilePath :: IORef (Maybe String)
                     }

-- a PairBox contains a VBox containing original text and annotation
data PairBox = PairBox { pbOrig :: Entry
                       , pbText :: Entry
                       , pbVbox :: VBox
                       }

data PairView = PairView { pvPairBox :: PairBox
                         , pvMargin  :: Label
                         , pvBox     :: HBox
                         }

type MenuFileOp = Window -> EditorView -> IO ()
