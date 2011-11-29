module FileIO (importFile, loadFile, saveFile) where

import qualified Data.Vector as V
import Data.IORef

import Editor (Editor, makePairBox, getPairs)
import Datafile

processFile :: (String -> [(String, String)]) -> Editor -> String -> IO ()
processFile f ed path = do
  s <- readFile path
  ps <- mapM makePairBox (f s)
  es <- return $ V.fromList ps
  writeIORef ed es

importFile = processFile parseImport
loadFile   = processFile parseFile

saveFile :: Editor -> String -> IO ()
saveFile ed path = do
  ls <- getPairs ed
  writeFile path (showFile ls)
