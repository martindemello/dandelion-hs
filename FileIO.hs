module FileIO (importFile, loadFile, saveFile, exportFile) where

import qualified Data.Vector as V
import Control.Monad ((>=>), (<=<), (>>), liftM)

import Editor (makePairBox, getPairs, setPairs, setFilePath, clearFilePath)
import Datafile
import Types

-- File -> Editor

fromRawFile, fromSavedFile :: String -> IO [PairBox]
fromRawFile = mapM makePairBox . parseImport
fromSavedFile = mapM makePairBox . parseFile

replaceFile :: (String -> IO [PairBox]) -> Editor -> String -> IO ()
replaceFile parser ed path = do
  (readFile >=> parser >=> setPairs ed) path

loadFile, importFile :: Editor -> String -> IO ()

loadFile ed path = do
  replaceFile fromSavedFile ed path
  setFilePath ed path

importFile ed path = do
  replaceFile fromRawFile ed path
  clearFilePath ed

-- Editor -> File

getText, getExport :: Editor -> IO String
getText ed = liftM showFile $ getPairs ed
getExport ed = liftM showExport $ getPairs ed

saveFile, exportFile :: Editor -> String -> IO ()

saveFile ed path = do
  getText ed >>= writeFile path
  setFilePath ed path

exportFile ed path = getExport ed >>= writeFile path
