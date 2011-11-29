module Editor where

import Graphics.UI.Gtk
import Data.IORef
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import GuiUtils

-- a PairBox contains a VBox containing a Label and an Entry
data PairBox = PairBox { pbOrig :: Label
                       , pbText :: Entry
                       , pbVbox :: VBox
                       }

type Editor = IORef (Vector PairBox)

newEditor :: IO Editor
newEditor = newIORef (V.empty)

-- just in case we change the internal representation of Editor
getContent :: Editor -> IO (Vector PairBox)
getContent ed = readIORef ed

-- pairbox functions
makePairBox :: (String, String) -> IO PairBox
makePairBox (l, s) = do
  box <- vBoxNew False 0
  label <- makeLabel l
  entry <- makeEntry s
  addToBox box label
  addToBox box entry
  return $ PairBox { pbOrig = label, pbText = entry, pbVbox = box }

origText :: PairBox -> IO String
origText pb = (labelGetText . pbOrig) pb

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
  es <- readIORef ed
  writeIORef ed (es `V.snoc` w)
  return ed

removeFromEditor :: Editor -> IO PairBox
removeFromEditor ed = do
  es <- readIORef ed
  e <- return $ V.last es
  writeIORef ed (V.init es)
  return e

addNewLine :: Editor -> String -> IO PairBox
addNewLine ed s = do
  l <- makePairBox (s, "")
  addToEditor ed l
  return l

getLine :: Editor -> Int -> IO PairBox
getLine ed i = do
  es <- readIORef ed
  return (es ! i)

getPairs :: Editor -> IO [(String, String)]
getPairs ed = do
  es <- getContent ed
  ls <- V.mapM pairOfPairBox es
  return $ V.toList ls
