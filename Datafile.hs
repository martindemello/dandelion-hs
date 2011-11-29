-- pure functions converting between file contents and  [(String, String)]

module Datafile (parseFile, parseImport, showFile, showExport) where

import Data.String.Utils (strip)
import Data.List (groupBy)

stringOfPair :: (String, String) -> String
stringOfPair (a,b) = ("# " ++ a ++ "\n> " ++ b)

joinLines :: [String] -> String
joinLines [] = ""
joinLines [x] = strip $ tail x
joinLines (x : xs) = (strip $ tail x) ++ "\n" ++ (joinLines xs)

collectLines :: [String] -> [String]
collectLines ls = map joinLines $ groupBy (\x y -> head x == head y) ls

parseLines :: [String] -> [(String, String)] -> [(String, String)]
parseLines [] a = a
parseLines [x] a = (x, "") : a
parseLines (x : y : xs) a = parseLines xs ((x, y) : a)

parseFile :: String -> [(String, String)]
parseFile s = reverse $ parseLines (collectLines $ lines s) []

parseImport :: String -> [(String, String)]
parseImport s = map (\x -> (x, "")) $ lines s

showFile :: [(String, String)] -> String
showFile ps = unlines $ map stringOfPair ps

showExport :: [(String, String)] -> String
showExport ps = unlines $ map snd ps
