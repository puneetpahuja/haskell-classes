module Main where

-- You have a bunch of numbers in a file, one per line. The file is too big
-- to be sorted in memory. You have to sort it on disk using merge sort.
--
-- Run this using...
-- ghc --make mergeSortOnDisk.hs -o mergesort

-- shuf -i 0-1000000000 -n 1000000 > numbers

import Data.List (sort)

main :: IO ()
main = putStrLn "Hello World"

sortAndSplit :: FilePath -> Int -> IO [FilePath]
sortAndSplit path chunkSize = do
  contents <- readFile path
  let
    numbers = map read . lines $ contents :: [Int]
    chunks = chunky chunkSize numbers
    sortedChunks = map sort chunks
    filepaths = map (("temp/"++) . show) [1..length chunks]
  writeLists sortedChunks filepaths
  return filepaths

chunky :: Int -> [a] -> [[a]]
chunky _ [] = []
chunky c xs = take c xs : chunky c (drop c xs)

writeLists :: Show a => [[a]] -> [FilePath] -> IO ()
writeLists [] _          = putStrLn "written all files"
writeLists _  []         = putStrLn "out of filepath"
writeLists (x:xs) (p:ps) = do
  writeList x p
  writeLists xs ps

writeList :: Show a => [a] -> FilePath -> IO ()
writeList xs path = do
  let
    string = unlines . map show $ xs
  writeFile path string

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y
  then x : merge xs (y:ys)
  else y : merge (x:xs) ys


mergeFiles :: [FilePath] -> FilePath -> IO ()
mergeFiles inPaths outPath = do
  contents <- traverse readFile inPaths
  let
    nss = map (map readInt . lines) contents
    sortedNs = foldl merge [] nss
    sortedNsString = unlines (map show sortedNs)
  writeFile outPath sortedNsString
  putStrLn "files merged"

readInt :: String -> Int
readInt = read


mergeSortFile :: FilePath -> FilePath -> Int -> IO ()
mergeSortFile inPath outPath chunkSize = do
  splittedFilePaths <- sortAndSplit inPath chunkSize
  mergeFiles splittedFilePaths outPath
