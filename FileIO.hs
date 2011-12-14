module FileIO (importFile, loadFile, saveFile) where

import qualified Data.Vector as V
import Data.IORef
import Control.Monad ((>=>), (<=<), (>>), liftM)

import Editor (Editor, PairBox, makePairBox, getPairs, setPairs, setFilename, clearFilename)
import Datafile

fromRawFile, fromSavedFile :: String -> IO [PairBox]
fromRawFile = mapM makePairBox . parseImport
fromSavedFile = mapM makePairBox . parseFile

replaceFile :: (String -> IO [PairBox]) -> Editor -> String -> IO ()
replaceFile parser ed path = do
  (readFile >=> parser >=> setPairs ed) path

loadFile, importFile :: Editor -> String -> IO ()
loadFile ed path = do
  replaceFile fromSavedFile ed path
  setFilename ed path

importFile ed path = do
  replaceFile fromRawFile ed path
  clearFilename ed

getText :: Editor -> IO String
getText ed = liftM showFile $ getPairs ed

saveFile :: Editor -> String -> IO ()
saveFile ed path = do
  getText ed >>= writeFile path
  setFilename ed path
