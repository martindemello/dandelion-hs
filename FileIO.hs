module FileIO (importFile, loadFile, saveFile) where

import qualified Data.Vector as V
import Data.IORef
import Control.Monad ((>=>), (<=<), liftM)

import Editor (Editor, PairBox, makePairBox, getPairs)
import Datafile

fromRawFile, fromSavedFile :: String -> IO [PairBox]
fromRawFile = mapM makePairBox . parseImport
fromSavedFile = mapM makePairBox . parseFile

setEditor :: Editor -> [PairBox] -> IO ()
setEditor ed = writeIORef ed . V.fromList

importFile, loadFile :: Editor -> String -> IO ()
importFile ed = readFile >=> fromRawFile >=> setEditor ed
loadFile ed = readFile >=> fromSavedFile >=> setEditor ed

getText :: Editor -> IO String
getText ed = liftM showFile $ getPairs ed

saveFile :: Editor -> String -> IO ()
saveFile ed path = getText ed >>= writeFile path
