module Editor where

import Graphics.UI.Gtk
import Data.IORef
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import GuiUtils

-- a PairBox contains a VBox containing original text and annotation
data PairBox = PairBox { pbOrig :: Entry
                       , pbText :: Entry
                       , pbVbox :: VBox
                       }

data Editor = Editor { edPairs    :: IORef (Vector PairBox)
                     , edFilePath :: IORef (Maybe String)
                     }

newEditor :: IO Editor
newEditor = do
  e <- newIORef (V.empty)
  f <- newIORef Nothing
  return $ Editor { edPairs = e, edFilePath = f }

-- just in case we change the internal representation of Editor
getContent :: Editor -> IO (Vector PairBox)
getContent ed = readIORef $ edPairs ed

-- pairbox functions
makePairBox :: (String, String) -> IO PairBox
makePairBox (l, s) = do
  box <- vBoxNew False 0
  orig <- makeEntry l
  text <- makeEntry s
  set orig [entryEditable := False, entryHasFrame := False, widgetCanFocus := False]
  widgetModifyBase orig StateNormal (Color 55000 60000 65535)
  entrySetHasFrame text True
  boxPackStart box orig PackNatural 0
  boxPackStart box text PackNatural 0
  return $ PairBox { pbOrig = orig, pbText = text, pbVbox = box }

origText :: PairBox -> IO String
origText pb = (entryGetText . pbOrig) pb

newText :: PairBox -> IO String
newText pb = (entryGetText . pbText) pb

pairOfPairBox :: PairBox -> IO (String, String)
pairOfPairBox pb = do
  a <- origText pb
  b <- newText pb
  return $ (a, b)

addPairToBox box v = addToBox box (pbVbox v)

-- editor functions
addToEditor :: Editor -> PairBox -> IO Editor
addToEditor ed w = do
  es <- getContent ed
  writeIORef (edPairs ed) (es `V.snoc` w)
  return ed

removeFromEditor :: Editor -> IO PairBox
removeFromEditor ed = do
  es <- getContent ed
  e <- return $ V.last es
  writeIORef (edPairs ed) (V.init es)
  return e

addNewLine :: Editor -> String -> IO PairBox
addNewLine ed s = do
  l <- makePairBox (s, "")
  addToEditor ed l
  return l

getLine :: Editor -> Int -> IO PairBox
getLine ed i = do
  es <- getContent ed
  return (es ! i)

getPairs :: Editor -> IO [(String, String)]
getPairs ed = do
  es <- getContent ed
  ls <- V.mapM pairOfPairBox es
  return $ V.toList ls

setPairs :: Editor -> [PairBox] -> IO ()
setPairs ed = writeIORef (edPairs ed) . V.fromList

getFilePath :: Editor -> IO (Maybe String)
getFilePath ed = readIORef $ edFilePath ed

setFilePath :: Editor -> String -> IO ()
setFilePath ed fname = writeIORef (edFilePath ed) (Just fname)

clearFilePath :: Editor -> IO ()
clearFilePath ed = writeIORef (edFilePath ed) Nothing
