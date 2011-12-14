module FileIO (importFile, loadFile, saveFile) where

import qualified Data.Vector as V
import Data.IORef
import Control.Monad ((>=>), (<=<), liftM)

import Editor (Editor, PairBox, makePairBox, getPairs, setPairs)
import Datafile

fromRawFile, fromSavedFile :: String -> IO [PairBox]
fromRawFile = mapM makePairBox . parseImport
fromSavedFile = mapM makePairBox . parseFile

importFile, loadFile :: Editor -> String -> IO ()
importFile ed = readFile >=> fromRawFile >=> setPairs ed
loadFile ed = readFile >=> fromSavedFile >=> setPairs ed

getText :: Editor -> IO String
getText ed = liftM showFile $ getPairs ed

saveFile :: Editor -> String -> IO ()
saveFile ed path = getText ed >>= writeFile path


