module Main where

import Data.List (sort)
import Data.List.Split (chunksOf)
import System.IO.Temp (writeSystemTempFile)

readI :: String -> Int
readI = read

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

main :: IO ()
main =
  getContents
  >>= return . map (unlines . map show . sort) . chunksOf 10000 . map readI . lines
  >>= mapM (writeSystemTempFile "hsort")
  >>= traverse readFile
  >>= return . unlines . map show . foldr1 merge . map (map readI . lines)
  >>= putStr
